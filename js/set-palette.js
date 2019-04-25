function init() {
  const varNames = [
    "color-primary",
    "color-primary-lighter",
    "color-nav-gradientstop",
    "color-background",
    "color-text",
    "color-backgroundlighter",
    "color-nav-link",
    "color-highlight"
  ];
  const host = document.getElementById("palette-host");
  const inputElements = varNames.map(name => {
    newInput = document.createElement("input");
    newInput.classList.add("palette-color");
    newInput.setAttribute("type", "color");
    newInput.setAttribute("data-colormap-name", name);
    host.appendChild(newInput);
    return newInput;
  });
  console.log(inputElements);

  const style = getComputedStyle(document.body);
  inputElements.forEach(element => {
    const varName = "--" + element.getAttribute("data-colormap-name");
    const colorValue = style.getPropertyValue(varName);
    console.log(`varName "${varName}" "${colorValue}"`);
    element.value = colorValue.trim();
    console.log("bind", colorValue, element);
  });

  inputElements.forEach(element => {
    element.addEventListener("input", e => {
      const varName = "--" + e.target.getAttribute("data-colormap-name");
      console.log(varName, e.target.value);
      document.body.style.setProperty(varName, e.target.value);
    });
  });
}

if (CSS.supports("color", "var(--fake-var)")) {
  init();
}
