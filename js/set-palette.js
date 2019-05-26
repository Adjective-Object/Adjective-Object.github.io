const palettes = {
  base: {
    "color-primary": "#4a8985",
    "color-primary-lighter": "#58a3c2",
    "color-background": "#efefef",
    "color-text": "#000000",
    "color-icon-stroke": "#000000",
    "color-backgroundlighter": "#000000",
    "color-nav-link": "#eeeeee",
    "color-nav-gradientstop": "#4f9199",
    "color-highlight": "#d74a6c"
  },
  dark: {
    "color-primary": "#ace7f3",
    "color-primary-lighter": "#43a9ab",
    "color-background": "#18191f",
    "color-text": "#eeeeee",
    "color-icon-stroke": "#e4e4e4",
    "color-backgroundlighter": "#000000",
    "color-nav-link": "#0f121c",
    "color-nav-gradientstop": "#1b75c0",
    "color-highlight": "#ff3886"
  },
  cherry: {
    "color-primary": "#ce6e79",
    "color-primary-lighter": "#e2a2b3",
    "color-background": "#fee0ff",
    "color-text": "#090531",
    "color-icon-stroke": "#e05e8c",
    "color-backgroundlighter": "#e59c9c",
    "color-nav-link": "#ffffff",
    "color-nav-gradientstop": "#ec85a6",
    "color-highlight": "#cb83a0"
  },
  forest: {
    "color-primary": "#75977c",
    "color-primary-lighter": "#a2e2a8",
    "color-background": "#faf6ea",
    "color-text": "#1e1e1e",
    "color-icon-stroke": "#533e39",
    "color-backgroundlighter": "#af875a",
    "color-nav-link": "#ffffff",
    "color-nav-gradientstop": "#443f56",
    "color-highlight": "#67a16d"
  }
};

const varNames = [
  "color-primary",
  "color-primary-lighter",
  "color-background",
  "color-text",
  "color-icon-stroke",
  "color-backgroundlighter",
  "color-nav-link",
  "color-nav-gradientstop",
  "color-highlight"
];

function initPaletteEditor() {
  const host = document.getElementById("palette-editor-host");
  const inputElements = varNames.map(name => {
    newInput = document.createElement("input");
    newInput.classList.add("palette-color");
    newInput.setAttribute("type", "color");
    newInput.setAttribute("data-colormap-name", name);
    newLabel = document.createElement("label");
    initPaletteEditor;
    newLabel.innerHTML = name;
    host.appendChild(newInput);
    host.appendChild(newLabel);
    return newInput;
  });

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
      document.body.style.setProperty(varName, e.target.value);
    });
  });

  const submitButton = document.createElement("button");
  submitButton.innerText = "Get Palette";
  submitButton.addEventListener("click", () => {
    const palette = {};
    inputElements.forEach(
      e => (palette[e.getAttribute("data-colormap-name")] = e.value)
    );
    alert(JSON.stringify(palette, null, 2));
  });
  host.appendChild(submitButton);

  host.classList.remove("inactive");
}

function loadPalette(paletteName) {
  const palette = palettes[paletteName];
  if (palette === undefined) {
    console.error(`no such palette ${paletteName}. Failing to load`);
    setQueryVar("theme", undefined);
    return;
  }

  for (let name in palette) {
    document.body.style.setProperty(`--${name}`, palette[name]);
    const inputColorElement = document.querySelector(
      `input[data-colormap-name=${name}]`
    );
    if (inputColorElement) {
      inputColorElement.value = palette[name];
    }
  }

  setQueryVar("theme", paletteName == "base" ? undefined : paletteName);
}

function initPaletteSwitcher() {
  const host = document.getElementById("palette-switcher-host");

  for (let paletteName in palettes) {
    const palette = palettes[paletteName];
    const paletteButton = document.createElement("a");
    paletteButton.classList.add("palette-selector");
    paletteButton.setAttribute("href", "#");

    paletteButton.innerText = paletteName;
    paletteButton.addEventListener("click", e => {
      e.preventDefault();
      loadPalette(paletteName);
    });

    host.appendChild(paletteButton);
    host.classList.remove("inactive");
  }
}

function getQueryVar(varName) {
  const query = window.location.search.substring(1);
  const queryVars = query.split("&");
  for (var i = 0; i < queryVars.length; i++) {
    var entry = queryVars[i].split("=");
    if (decodeURIComponent(entry[0]) == varName) {
      return entry.length == 2 ? decodeURIComponent(entry[1]) : true;
    }
  }
  return null;
}

function setQueryVar(varName, value) {
  console.log("setQueryVar", varName, value);
  const query = window.location.search.substring(1);
  let encountered = false;
  const encodedQueryVarString = `${encodeURIComponent(
    varName
  )}=${encodeURIComponent(value)}`;
  const replacedQueryString = query
    .split("&")
    .map(entryString => {
      let rawEntry = entryString.split("=");
      if (decodeURIComponent(rawEntry[0]) === varName) {
        encountered = true;
        if (value !== undefined) {
          return encodedQueryVarString;
        }
        return "";
      }
      return entryString;
    })
    .reduce((a, b) => (a != null ? a + "&" + b : b));

  const updatedQueryString = encountered
    ? replacedQueryString
    : replacedQueryString.length
    ? replacedQueryString + "&" + encodedQueryVarString
    : encodedQueryVarString;

  const updatedSearchString = updatedQueryString.length
    ? "?" + updatedQueryString
    : "";

  const newUrl =
    window.location.protocol +
    "//" +
    window.location.host +
    window.location.pathname +
    updatedSearchString;

  window.history.pushState({ path: newUrl }, "", newUrl);
}

if (CSS.supports("color", "var(--fake-var)")) {
  if (getQueryVar("themeEditor") !== null) {
    initPaletteEditor();
  }
  initPaletteSwitcher();
  const initialTheme = getQueryVar("theme");
  if (initialTheme !== null) {
    document.body.classList.add("notransition");
    loadPalette(initialTheme);
    requestAnimationFrame(() => {
      document.body.classList.remove("notransition");
    });
  }
}
