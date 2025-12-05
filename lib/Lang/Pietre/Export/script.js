var highlight = function (declid, on) {
	return function () {
		var spans = document.getElementsByTagName('span');
		for (var i = 0; i < spans.length; i++) {
			var that = spans[i];

			if (declid != that.declid) {
				continue;
			}

			if (on) {
				that.classList.add("syntax-hover-highlight");
			} else {
				that.classList.remove("syntax-hover-highlight");
			}
		}
	}
};

window.onload = function () {
	var spans = document.getElementsByTagName('span');
	for (var i = 0; i < spans.length; i++) {
        var that = spans[i];
        if (that.declid) {
		    that.onmouseover = highlight(that.declid, true);
		    that.onmouseout = highlight(that.declid, false);
        }
	}
};
