const highlight = (declid, enable) => {
  const spans = document.querySelectorAll(`[declid=${declid}]`);
  spans.forEach((element) => {
    element.classList.toggle("syntax-hover-highlight", enable);
  });
};

window.onload = function () {
  const spans = document.querySelectorAll(`[declid]`);
  spans.forEach((element) => {
    element.addEventListener("mouseover", (_) => {
      highlight(element.getAttribute("declid"), true);
    });

    element.addEventListener("mouseout", (_) => {
      highlight(element.getAttribute("declid"), false);
    });
  });
};
