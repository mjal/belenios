function NiceTextInput(props={}){
  return e(
    "input",
    {
      type: "text",
      className: "nice-text-input",
      ...props
    }
  );
}

export { NiceTextInput };
export default NiceTextInput;
