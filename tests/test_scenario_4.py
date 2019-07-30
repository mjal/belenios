#!/usr/bin/python
# coding: utf-8
import unittest
import random
import os
import subprocess
import re
import sys
from uuid import uuid4
from distutils.util import strtobool
from selenium.common.exceptions import UnexpectedAlertPresentException
from util.fake_sent_emails_manager import FakeSentEmailsManager
from util.selenium_tools import wait_for_element_exists, wait_for_elements_exist, wait_for_element_exists_and_contains_expected_text, wait_for_element_exists_and_has_non_empty_content, wait_for_an_element_with_partial_link_text_exists, set_element_attribute, wait_for_element_exists_and_has_non_empty_attribute
from util.election_testing import console_log, random_email_addresses_generator, remove_database_folder, wait_a_bit, build_css_selector_to_find_buttons_in_page_content_by_value, initialize_server, initialize_browser, election_page_url_to_election_id, verify_election_consistency, create_election_data_snapshot, delete_election_data_snapshot, log_in_as_administrator, log_out, administrator_starts_creation_of_election, administrator_edits_election_questions, administrator_sets_election_voters, administrator_validates_creation_of_election
from util.election_test_base import BeleniosElectionTestBase
from test_scenario_2 import BeleniosTestElectionScenario2Base, initialize_browser_for_scenario_2
import settings


class BeleniosTestElectionScenario4(BeleniosTestElectionScenario2Base):
    """
    Properties:
    - server
    - browser
    - fake_sent_emails_manager: An instance of FakeSentEmailsManager
    - voters_email_addresses: A list of email addresses (strings). This is all users who are invited to vote
    - voters_email_addresses_who_have_lost_their_password: A list of email addresses (strings). This is all users who have asked for a new password.
    - voters_email_addresses_who_have_voted: A dictionary, indexed by email address (string), where each element value is True
    - voters_data: A dictionary, indexed by email address (string), where each element is a dictionary of fields for the voter who is identified by this email address. This is data about all users who have voted.
    - election_page_url: The election page URL (string). Example: "http://localhost:8001/elections/H5ecRG3wHZ21cp/"
    - election_id: The election ID (string). Example: "H5ecRG3wHZ21cp"
    - draft_election_administration_page_url: URL of the draft election administration page
    - credential_authority_link
    - credential_authority_file_paths
    - links_for_trustees
    - downloaded_files_paths_per_trustee
    - temporary_files_to_remove_after_test
    - encrypted_tally_hash
    - closed_election_tally_links_for_trustees
    """

    __test__ = True


    def __init__(self, *args, **kw):
        super().__init__(*args, **kw)

        self.draft_election_administration_page_url = None
        self.credential_authority_link = None
        self.credential_authority_file_paths = dict() # A dict where key is a label describing the file and value is the absolute path to file
        self.links_for_trustees = []
        self.downloaded_files_paths_per_trustee = dict() # A dict where key is trustee email address, and value is a dict where key is file label (for example "private key" or "public key"), and value is the absolute path to the file
        self.temporary_files_to_remove_after_test = []
        self.encrypted_tally_hash = None
        self.closed_election_tally_links_for_trustees = []


    def administrator_invites_trustees_and_sets_threshold(self):
        self.browser = initialize_browser_for_scenario_2()
        browser = self.browser

        log_in_as_administrator(browser)

        browser.get(self.draft_election_administration_page_url)

        wait_a_bit()

        # In the trustees section, she clicks on the "here" link
        setup_election_key_link_label = "here"
        setup_election_key_link_element = wait_for_an_element_with_partial_link_text_exists(browser, setup_election_key_link_label)
        setup_election_key_link_element.click()

        wait_a_bit()

        # She clicks on the "threshold mode" link
        threshold_mode_link_label = "threshold mode"
        threshold_mode_link_element = wait_for_an_element_with_partial_link_text_exists(browser, threshold_mode_link_label)
        threshold_mode_link_element.click()

        wait_a_bit()

        # She adds `NUMBER_OF_TRUSTEES` trustees (their email address), and remembers the link she will send to each trustee
        # (The threshold field appears only after user has added the first trustee)
        self.links_for_trustees = []
        email_address_field_css_selector = "#main form input[name=__co_eliom_id]" # TODO: Maybe we should edit Belenios' HTML template to rename `__co_eliom_id` to something more explicit, like `__co_eliom_new_trustee_email_address`
        submit_button_css_selector = "#main form input[type=submit][value=Add]"

        for idx, email_address in enumerate(settings.TRUSTEES_EMAIL_ADDRESSES):
            email_address_field_element = wait_for_element_exists(browser, email_address_field_css_selector)
            email_address_field_element.clear()
            email_address_field_element.send_keys(email_address)

            submit_button_element = wait_for_element_exists(browser, submit_button_css_selector)
            submit_button_element.click()

            trustee_link_css_selector = "#main table tbody tr:nth-of-type(" + str(idx + 2) + ") td:nth-of-type(3) a"
            trustee_link_element = wait_for_element_exists_and_has_non_empty_content(browser, trustee_link_css_selector)
            self.links_for_trustees.append(trustee_link_element.get_attribute('href'))

            wait_a_bit()

        # In the field next to "Threshold:", she types the value of `U` (aka `TRUSTEES_THRESHOLD_VALUE`)
        threshold_value_field_css_selector = "#main form input[name=__co_eliom_threshold]"
        threshold_value_field_element = wait_for_element_exists(browser, threshold_value_field_css_selector, settings.EXPLICIT_WAIT_TIMEOUT)
        threshold_value_field_value = settings.TRUSTEES_THRESHOLD_VALUE
        threshold_value_field_element.clear()
        threshold_value_field_element.send_keys(threshold_value_field_value)

        wait_a_bit()

        # She clicks on the "Set" button
        submit_button_css_selector = "#main form input[type=submit][value=Set]"
        submit_button_element = wait_for_element_exists(browser, submit_button_css_selector)
        submit_button_element.click()

        wait_a_bit()

        # (She checks that in the table, the "STATE" column is "1a" on every row)
        # TODO

        # She sends to each trustee an email containing their own link
        subject = "Link to generate the decryption key"
        content_format = """\
Dear trustee,

You will find below the link to generate your private decryption key, used to tally the election.

{link_for_trustee}

Here's the instructions:
1. click on the link
2. click on "generate a new key pair"
3. your private key will appear in another window or tab. Make sure
you SAVE IT properly otherwise it will not possible to tally and the
election will be canceled.
4. in the first window, click on "submit" to send the public part of
your key, used encrypt the votes. For verification purposes, you
should save this part (that starts with "pok" "challenge"), for
example sending yourself an email.

Regarding your private key, it is crucial you save it (otherwise the
election will be canceled) and store it securely (if your private key
is known together with the private keys of the other trustees, then
vote privacy is no longer guaranteed). We suggest two options:
1. you may store the key on a USB stick and store it in a safe.
2. Or you may simply print it and store it in a safe.
Of course, more cryptographic solutions are welcome as well.

Thank you for your help,

--
The election administrator.\
"""
        for idx, trustee_email_address in enumerate(settings.TRUSTEES_EMAIL_ADDRESSES):
            custom_content = content_format.format(link_for_trustee=self.links_for_trustees[idx])
            self.fake_sent_emails_manager.send_email(settings.ADMINISTRATOR_EMAIL_ADDRESS, trustee_email_address, subject, custom_content)

        # Optionnaly, she logs out
        # log_out(browser)

        # She closes the window
        browser.quit()


    def trustees_do_initialization_step_1_of_3(self):
        # Trustees initialization step 1/3: Trustees generate election private keys. Each of the `T` (aka `NUMBER_OF_TRUSTEES`) trustees will do the following process:
        for idx, trustee_email_address in enumerate(settings.TRUSTEES_EMAIL_ADDRESSES):
            # Trustee opens link that has been sent to him by election administrator
            link_for_this_trustee = self.links_for_trustees[idx] # TODO: Decide either not send trustee email at all or read trustee link from email content
            self.browser = initialize_browser_for_scenario_2()
            browser = self.browser
            browser.get(link_for_this_trustee)

            wait_a_bit()

            # He checks that the page content shows the same election URL as the one the administrator saw
            election_url_css_selector = "#main ul li"
            election_url_element = wait_for_element_exists_and_has_non_empty_content(browser, election_url_css_selector)
            election_url_content = election_url_element.get_attribute('innerText').strip()
            assert election_url_content == self.election_page_url

            # He clicks on the "Generate private key" button
            generate_button_css_selector = "#interactivity button"
            generate_button_expected_label = "Generate private key"
            generate_button_element = wait_for_element_exists_and_contains_expected_text(browser, generate_button_css_selector, generate_button_expected_label)
            generate_button_element.click()

            wait_a_bit()

            # He clicks on the "private key" link, to download the private key (file is saved by default as `private_key.txt`)
            link_css_ids = ["private_key"]
            link_expected_labels = ["private key"]
            self.downloaded_files_paths_per_trustee[trustee_email_address] = dict()
            for idx2, link_css_id in enumerate(link_css_ids):
                link_target_filename = str(uuid4())
                set_element_attribute(browser, link_css_id, 'download', link_target_filename)
                link_expected_label = link_expected_labels[idx2]
                link_element = wait_for_an_element_with_partial_link_text_exists(browser, link_expected_label)
                assert link_element.get_attribute('id') == link_css_id
                link_element.click()
                file_absolute_path = os.path.join(settings.BROWSER_DOWNLOAD_FOLDER, link_target_filename)
                # We save the filename in a class instance property, so that we can import the file afterwards (during partial decryption step)
                self.downloaded_files_paths_per_trustee[trustee_email_address][link_expected_labels[idx2]] = file_absolute_path
                self.remember_temporary_file_to_remove_after_test(file_absolute_path)

            wait_a_bit()

            # He clicks on the "Submit" button
            submit_button_expected_label = "Submit"
            submit_button_css_selector = "#main input[type=submit][value='" + submit_button_expected_label + "']"
            submit_button_element = wait_for_element_exists(browser, submit_button_css_selector)
            submit_button_element.click()

            wait_a_bit()

            # He checks that the next page shows the expected confirmation sentence (If trustee was the last one in the list, he checks that page contains text "Now, all the certificates of the trustees have been generated. Proceed to generate your share of the decryption key.", else he checks for sentence "Waiting for the other trustees... Reload the page to check progress.")
            if idx == settings.NUMBER_OF_TRUSTEES - 1:
                expected_confirmation_label = "Now, all the certificates of the trustees have been generated. Proceed to generate your share of the decryption key."
            else:
                expected_confirmation_label = "Waiting for the other trustees... Reload the page to check progress."
            expected_confirmation_css_selector = "#main"
            wait_for_element_exists_and_contains_expected_text(browser, expected_confirmation_css_selector, expected_confirmation_label)

            wait_a_bit()

            # He closes the window
            browser.quit()

            # (Administrator logs in, selects the election by clicking on its link, and in the "Trustees" section clicks on "here". She checks that in the table on the current trustee row, the "STATE" column is now "1b" instead of "1a")
            # TODO


    def trustees_do_initialization_step_2_of_3(self):
        # Trustees initialization step 2/3: Trustees generate their share of the decryption key. Each of the `T` (aka `NUMBER_OF_TRUSTEES`) trustees will do the following process:
        for idx, trustee_email_address in enumerate(settings.TRUSTEES_EMAIL_ADDRESSES):
            # Trustee opens link that has been sent to him by election administrator
            link_for_this_trustee = self.links_for_trustees[idx] # TODO: Decide either not send trustee email at all or read trustee link from email content
            self.browser = initialize_browser_for_scenario_2()
            browser = self.browser
            browser.get(link_for_this_trustee)

            wait_a_bit()

            # He checks that the page content shows the same election URL as the one the administrator saw
            election_url_css_selector = "#main ul li"
            election_url_element = wait_for_element_exists_and_has_non_empty_content(browser, election_url_css_selector)
            election_url_content = election_url_element.get_attribute('innerText').strip()
            assert election_url_content == self.election_page_url

            # He checks the presence of text "Now, all the certificates of the trustees have been generated. Proceed to generate your share of the decryption key."
            expected_confirmation_label = "Now, all the certificates of the trustees have been generated. Proceed to generate your share of the decryption key."
            expected_confirmation_css_selector = "#main"
            wait_for_element_exists_and_contains_expected_text(browser, expected_confirmation_css_selector, expected_confirmation_label)

            # In field next to "Enter your private key:", he types the content of the `private_key.txt` file he downloaded
            private_key_storage_label = "private key"
            private_key_file = self.downloaded_files_paths_per_trustee[trustee_email_address][private_key_storage_label]
            private_key_css_selector = "#compute_private_key"
            private_key_element = wait_for_element_exists(browser, private_key_css_selector)
            private_key_element.clear()
            with open(private_key_file) as myfile:
                private_key_element.send_keys(myfile.read())

            wait_a_bit()

            # He clicks on the "Proceed" button
            proceed_button_css_selector = "#compute_button"
            proceed_button_element = wait_for_element_exists(browser, proceed_button_css_selector)
            proceed_button_element.click()

            # He waits until the text field next to "Data:" contains text, and clicks on the "Submit" button
            data_field_css_selector = "#compute_data"
            data_field_expected_non_empty_attribute = "value"
            wait_for_element_exists_and_has_non_empty_attribute(browser, data_field_css_selector, data_field_expected_non_empty_attribute)

            submit_button_expected_label = "Submit"
            submit_button_css_selector = "#compute_form input[type=submit][value=" + submit_button_expected_label + "]"
            submit_button_element = wait_for_element_exists(browser, submit_button_css_selector)
            submit_button_element.click()

            wait_a_bit()

            # If he is not the last trustee in the list, he checks that the next page contains text "Waiting for the other trustees... Reload the page to check progress.". Else, he checks that the next page contains text "Now, all the trustees have generated their secret shares. Proceed to the final checks so that the election can be validated."
            if idx == settings.NUMBER_OF_TRUSTEES - 1:
                expected_confirmation_label = "Now, all the trustees have generated their secret shares. Proceed to the final checks so that the election can be validated."
            else:
                expected_confirmation_label = "Waiting for the other trustees... Reload the page to check progress."
            expected_confirmation_css_selector = "#main"
            wait_for_element_exists_and_contains_expected_text(browser, expected_confirmation_css_selector, expected_confirmation_label)

            wait_a_bit()

            # He closes the window
            browser.quit()

            # (Administrator logs in, selects the election by clicking on its link, and in the "Trustees" section clicks on "here". She checks that in the table on the current trustee row, the "STATE" column is now "2b" instead of "2a")
            # TODO

    def trustees_do_initialization_step_3_of_3(self):
        # Trustees initialization step 3/3: Trustees do the final checks so that the election can be validated. Each of the `T` (aka `NUMBER_OF_TRUSTEES`) trustees will do the following process:
        for idx, trustee_email_address in enumerate(settings.TRUSTEES_EMAIL_ADDRESSES):
            # TODO
            # Trustee opens link that has been sent to him by election administrator
            # He checks that the page content shows the same election URL as the one the administrator saw
            # He checks the presence of text "Step 3/3"
            # He checks the presence of text "Now, all the certificates of the trustees have been generated. Proceed to generate your share of the decryption key."
            # In field next to "Enter your private key:", he types the content of the `private_key.txt` file he downloaded
            # He clicks on the "Proceed" button
            # He waits until the text field next to "Data:" contains text, and clicks on the "Submit" button
            # He checks that the next page contains text "Your job in the key establishment protocol is done!"
            # He clicks on the "public key" link and downloads the file (file is saved by default as `public_key.json`)
            # (Administrator logs in, selects the election by clicking on its link, and in the "Trustees" section clicks on "here". She checks that in the table on the current trustee row, the "STATE" column is now "3b" instead of "3a")
            pass


    def test_scenario_3_manual_vote_with_threshold(self):
        console_log("### Running test method BeleniosTestElectionScenario4::test_scenario_3_manual_vote_with_threshold()")
        console_log("### Starting step: administrator_starts_creation_of_manual_election")
        self.administrator_starts_creation_of_manual_election()
        console_log("### Step complete: administrator_starts_creation_of_manual_election")

        console_log("### Starting step: credential_authority_sends_credentials_to_voters")
        self.credential_authority_sends_credentials_to_voters()
        console_log("### Step complete: credential_authority_sends_credentials_to_voters")

        console_log("### Starting step: administrator_invites_trustees_and_sets_threshold")
        self.administrator_invites_trustees_and_sets_threshold()
        console_log("### Step complete: administrator_invites_trustees_and_sets_threshold")

        console_log("### Starting step: trustees_do_initialization_step_1_of_3")
        self.trustees_do_initialization_step_1_of_3()
        console_log("### Step complete: trustees_do_initialization_step_1_of_3")

        # (Administrator logs in, selects the election by clicking on its link, and in the "Trustees" section clicks on "here". She checks that in the table on every row, the "STATE" column is now "2a")
        # TODO

        console_log("### Starting step: trustees_do_initialization_step_2_of_3")
        self.trustees_do_initialization_step_2_of_3()
        console_log("### Step complete: trustees_do_initialization_step_2_of_3")

        console_log("### Starting step: trustees_do_initialization_step_3_of_3")
        self.trustees_do_initialization_step_3_of_3()
        console_log("### Step complete: trustees_do_initialization_step_3_of_3")

        # console_log("### Starting step: administrator_completes_creation_of_election")
        # self.administrator_completes_creation_of_election()
        # console_log("### Step complete: administrator_completes_creation_of_election")

        # console_log("### Starting step: verify_election_consistency using `belenios_tool verify` (0)")
        # verify_election_consistency(self.election_id)
        # console_log("### Step complete: verify_election_consistency using `belenios_tool verify` (0)")

        # console_log("### Starting step: all_voters_vote_in_sequences")
        # self.all_voters_vote_in_sequences()
        # console_log("### Step complete: all_voters_vote_in_sequences")

        # console_log("### Starting step: verify_election_consistency using `belenios_tool verify` (1)")
        # verify_election_consistency(self.election_id)
        # console_log("### Step complete: verify_election_consistency using `belenios_tool verify` (1)")

        # console_log("### Starting step: create_election_data_snapshot (0)")
        # snapshot_folder = create_election_data_snapshot(self.election_id)
        # console_log("### Step complete: create_election_data_snapshot (0)")

        # try:
        #     console_log("### Starting step: some_voters_revote")
        #     self.some_voters_revote()
        #     console_log("### Step complete: some_voters_revote")

        #     console_log("### Starting step: verify_election_consistency using `belenios_tool verify-diff` (0)")
        #     verify_election_consistency(self.election_id, snapshot_folder)
        # finally:
        #     delete_election_data_snapshot(snapshot_folder)
        # console_log("### Step complete: verify_election_consistency using `belenios_tool verify-diff` (0)")

        # console_log("### Starting step: verify_election_consistency using `belenios_tool verify` (2)")
        # verify_election_consistency(self.election_id)
        # console_log("### Step complete: verify_election_consistency using `belenios_tool verify` (2)")

        # console_log("### Starting step: administrator_starts_tallying_of_election")
        # self.administrator_starts_tallying_of_election()
        # console_log("### Step complete: administrator_starts_tallying_of_election")

        # console_log("### Starting step: trustees_do_partial_decryption")
        # self.trustees_do_partial_decryption()
        # console_log("### Step complete: trustees_do_partial_decryption")

        # console_log("### Starting step: administrator_finishes_tallying_of_election")
        # self.administrator_finishes_tallying_of_election()
        # console_log("### Step complete: administrator_finishes_tallying_of_election")

        # console_log("### Starting step: verify_election_consistency using `belenios_tool verify` (3)")
        # verify_election_consistency(self.election_id)
        # console_log("### Step complete: verify_election_consistency using `belenios_tool verify` (3)")


if __name__ == "__main__":
    random_seed = os.getenv('RANDOM_SEED', None)
    if not random_seed:
        random_seed = random.randrange(sys.maxsize)
    console_log("Python random seed being used:", random_seed)
    random.seed(random_seed)

    if os.getenv('USE_HEADLESS_BROWSER', None):
        settings.USE_HEADLESS_BROWSER = bool(strtobool(os.getenv('USE_HEADLESS_BROWSER')))

    settings.SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH = os.getenv('SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH', settings.SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH)
    settings.WAIT_TIME_BETWEEN_EACH_STEP = float(os.getenv('WAIT_TIME_BETWEEN_EACH_STEP', settings.WAIT_TIME_BETWEEN_EACH_STEP))
    settings.EXPLICIT_WAIT_TIMEOUT = int(os.getenv('EXPLICIT_WAIT_TIMEOUT', settings.EXPLICIT_WAIT_TIMEOUT))
    settings.NUMBER_OF_INVITED_VOTERS = int(os.getenv('NUMBER_OF_INVITED_VOTERS', settings.NUMBER_OF_INVITED_VOTERS))
    settings.NUMBER_OF_VOTING_VOTERS = int(os.getenv('NUMBER_OF_VOTING_VOTERS', settings.NUMBER_OF_VOTING_VOTERS))
    settings.NUMBER_OF_REVOTING_VOTERS = int(os.getenv('NUMBER_OF_REVOTING_VOTERS', settings.NUMBER_OF_REVOTING_VOTERS))
    settings.NUMBER_OF_REGENERATED_PASSWORD_VOTERS = int(os.getenv('NUMBER_OF_REGENERATED_PASSWORD_VOTERS', settings.NUMBER_OF_REGENERATED_PASSWORD_VOTERS))
    settings.ADMINISTRATOR_USERNAME = os.getenv('ADMINISTRATOR_USERNAME', settings.ADMINISTRATOR_USERNAME)
    settings.ADMINISTRATOR_PASSWORD = os.getenv('ADMINISTRATOR_PASSWORD', settings.ADMINISTRATOR_PASSWORD)
    settings.ELECTION_TITLE = os.getenv('ELECTION_TITLE', settings.ELECTION_TITLE)
    settings.ELECTION_DESCRIPTION = os.getenv('ELECTION_DESCRIPTION', settings.ELECTION_DESCRIPTION)
    settings.INITIATOR_CONTACT = os.getenv('INITIATOR_CONTACT', settings.INITIATOR_CONTACT)
    settings.BROWSER_DOWNLOAD_FOLDER = os.getenv('BROWSER_DOWNLOAD_FOLDER', settings.BROWSER_DOWNLOAD_FOLDER)
    settings.ADMINISTRATOR_EMAIL_ADDRESS = os.getenv('ADMINISTRATOR_EMAIL_ADDRESS', settings.ADMINISTRATOR_EMAIL_ADDRESS)
    settings.CREDENTIAL_AUTHORITY_EMAIL_ADDRESS = os.getenv('CREDENTIAL_AUTHORITY_EMAIL_ADDRESS', settings.CREDENTIAL_AUTHORITY_EMAIL_ADDRESS)
    settings.NUMBER_OF_TRUSTEES = int(os.getenv('NUMBER_OF_TRUSTEES', settings.NUMBER_OF_TRUSTEES))
    # TODO: settings.TRUSTEES_EMAIL_ADDRESSES (it cannot be manipulated the same way because it is an array)
    settings.TRUSTEES_THRESHOLD_VALUE = os.getenv('TRUSTEES_THRESHOLD_VALUE', settings.TRUSTEES_THRESHOLD_VALUE)

    console_log("USE_HEADLESS_BROWSER:", settings.USE_HEADLESS_BROWSER)
    console_log("SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH:", settings.SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH)
    console_log("WAIT_TIME_BETWEEN_EACH_STEP:", settings.WAIT_TIME_BETWEEN_EACH_STEP)
    console_log("EXPLICIT_WAIT_TIMEOUT:", settings.EXPLICIT_WAIT_TIMEOUT)
    console_log("NUMBER_OF_INVITED_VOTERS:", settings.NUMBER_OF_INVITED_VOTERS)
    console_log("NUMBER_OF_VOTING_VOTERS:", settings.NUMBER_OF_VOTING_VOTERS)
    console_log("NUMBER_OF_REVOTING_VOTERS:", settings.NUMBER_OF_REVOTING_VOTERS)
    console_log("NUMBER_OF_REGENERATED_PASSWORD_VOTERS:", settings.NUMBER_OF_REGENERATED_PASSWORD_VOTERS)
    console_log("ELECTION_TITLE:", settings.ELECTION_TITLE)
    console_log("ELECTION_DESCRIPTION:", settings.ELECTION_DESCRIPTION)
    console_log("INITIATOR_CONTACT:", settings.INITIATOR_CONTACT)
    console_log("BROWSER_DOWNLOAD_FOLDER:", settings.BROWSER_DOWNLOAD_FOLDER)
    console_log("ADMINISTRATOR_EMAIL_ADDRESS:", settings.ADMINISTRATOR_EMAIL_ADDRESS)
    console_log("CREDENTIAL_AUTHORITY_EMAIL_ADDRESS:", settings.CREDENTIAL_AUTHORITY_EMAIL_ADDRESS)
    console_log("TRUSTEES_EMAIL_ADDRESSES:", settings.TRUSTEES_EMAIL_ADDRESSES)
    console_log("NUMBER_OF_TRUSTEES:", settings.NUMBER_OF_TRUSTEES)
    console_log("TRUSTEES_THRESHOLD_VALUE:", settings.TRUSTEES_THRESHOLD_VALUE)

    unittest.main()
