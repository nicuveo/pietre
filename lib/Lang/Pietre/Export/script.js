const highlight = (declid, enable) => {
  const spans = document.querySelectorAll(`[declid=${declid}]`);
  for (let i = 0; i < spans.length; i++) {
    spans[i].classList.toggle("syntax-hover-highlight", enable);
  }
};

window.onload = function () {
  const spans = document.querySelectorAll("span");
  spans.forEach((element) => {
    element.addEventListener("mouseover", (_) => {
      highlight(element.getAttribute("declid"), true);
    });

    element.addEventListener("mouseout", (_) => {
      highlight(element.getAttribute("declid"), false);
    });
  });
};
