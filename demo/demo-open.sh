#!/bin/bash

set -e

export BELENIOS_USE_URANDOM=1

BELENIOS=${BELENIOS:-$PWD}

belenios-tool () {
    $BELENIOS/_build/belenios-tool "$@"
}

header () {
    echo
    echo "=-=-= $1 =-=-="
    echo
}

header "Setup election"

UUID=`uuidgen`
echo "UUID of the election is $UUID"

DIR=$BELENIOS/demo/data/$UUID
mkdir $DIR
cd $DIR

# Common options
uuid="--uuid $UUID"
group="--group $BELENIOS/demo/groups/rfc3526-2048.json"

# Generate credentials
belenios-tool credgen $uuid $group --count 5
mv *.pubcreds public_creds.txt
mv *.privcreds private_creds.txt

# Generate trustee keys
belenios-tool trustee-keygen $group
belenios-tool trustee-keygen $group
belenios-tool trustee-keygen $group
cat *.pubkey > public_keys.jsons

# Generate election parameters
belenios-tool mkelection $uuid $group --template $BELENIOS/demo/templates/questions-open.json

header "Simulate votes"

cat > votes.txt <<EOF
[[1,0],[1,0,0],[1,2,3]]
[[1,0],[0,1,0],[6,5,4]]
[[0,1],[0,0,1],[7,9,8]]
[[1,0],[1,0,0],[11,10,12]]
[[0,0],[0,1,0],[15,13,14]]
EOF

paste private_creds.txt votes.txt | while read id cred vote; do
    belenios-tool vote --privcred <(echo "$cred") --ballot <(echo "$vote")
    echo "Voter $id voted" >&2
    echo >&2
done > ballots.tmp
mv ballots.tmp ballots.jsons

header "Perform verification"

belenios-tool verify

header "Simulate and verify update"

tdir="$(mktemp -d)"
cp election.json public_creds.txt public_keys.jsons "$tdir"
head -n3 ballots.jsons > "$tdir/ballots.jsons"
belenios-tool verify-diff --dir1="$tdir" --dir2=.
rm -rf "$tdir"

header "Shuffle ciphertexts"

belenios-tool shuffle > shuffles.jsons
echo >&2
belenios-tool shuffle >> shuffles.jsons

header "Perform decryption"

for u in *.privkey; do
    belenios-tool decrypt --privkey $u
    echo >&2
done > partial_decryptions.tmp
mv partial_decryptions.tmp partial_decryptions.jsons

header "Finalize tally"

belenios-tool validate
rm -f shuffles.jsons

header "Perform final verification"

belenios-tool verify

echo
echo "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="
echo
echo "The simulated election was successful! Its result can be seen in"
echo "  $DIR/result.json"
echo
echo "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="
echo