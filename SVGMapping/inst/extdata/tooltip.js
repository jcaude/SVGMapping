// Tooltip system
// -----------------------------

function legacyTooltip(evt,text) {

    // locals
    var h_max = 30;
    var w_max = 400;
    var w_margin = 20;
    var h_margin = 8;

    ww = svgDocument.rootElement.getBBox().width;
    wh = svgDocument.rootElement.getBBox().height;

    // build a text item and adjust to BBox
    svg_text = svgDocument.createElementNS(svgNS, "text")
    svg_text.appendChild(svgDocument.createTextNode(text))
    svg_text.setAttributeNS(null,"font-size", "80%")
    svgDocument.rootElement.appendChild(svg_text)
    var text_w = svg_text.getComputedTextLength()
    var text_len = text.length
    while(text_w + w_margin > w_max && text_w > 10) {
	text_len = text_len - 1
	svg_text.firstChild.replaceWholeText(text.substr(0, l) + "...")
	text_w = svg_text.getComputedTextLength()
    }
    text_h = h_max
    svgDocument.rootElement.removeChild(svg_text)
    
    // compute final size
    var w = text_w + w_margin
    var h = text_h + h_margin

    // create & insert the background rectangle
    bg_rect = svgDocument.createElementNS(svgNS, "rect")
    bg_rect.setAttributeNS(null, "width", w)
    bg_rect.setAttributeNS(null, "height", h)
    bg_rect.setAttributeNS(null, "fill", "#eeeeee")
    bg_rect.setAttributeNS(null, "stroke", "black")
    svgDocument.rootElement.appendChild(bg_rect)
    uiItems.push(bg_rect)

    // insert the text element (again)
    svgDocument.rootElement.appendChild(svg_text)
    uiItems.push(svg_text)

    // compute tooltip (best) position
    x = evt.clientX + 9 + window.pageXOffset;
    y = evt.clientY + 17 + window.pageYOffset;
    if (x + w > ww) {
	// The tooltip is out of the SVG document (on the right), 
	// so display it on the left of the mouse cursor
	x = x - w;
    }
    if (y + h > wh) {
	// Same idea, when tooltip is below the document bottom
	y = y - h - 25;
    }

    // Put all elements on the right place
    bg_rect.setAttributeNS(null, "x", x)
    bg_rect.setAttributeNS(null, "y", y)
    svg_text.setAttributeNS(null, "x", x + w_margin/2)
    svg_text.setAttributeNS(null, "y", y + h_max/2 + h_margin/2)
    
}

function displayAnnotation(evt, name, description, foldchanges, colors)
{
    var n = foldchanges.length

    var h = 110;
    var w = 400;

    ww = svgDocument.rootElement.getBBox().width;
    wh = svgDocument.rootElement.getBBox().height;

    bgrect = svgDocument.createElementNS(svgNS, "rect")
    bgrect.setAttributeNS(null, "width", w)
    bgrect.setAttributeNS(null, "height", h)
    bgrect.setAttributeNS(null, "fill", "#eeeeee")
    bgrect.setAttributeNS(null, "stroke", "black")
    svgDocument.rootElement.appendChild(bgrect)
    uiItems.push(bgrect);

    if (description != null) {
	descel = svgDocument.createElementNS(svgNS, "text")
	descel.appendChild(svgDocument.createTextNode(description))
	descel.setAttributeNS(null, "font-size", "80%")
	svgDocument.rootElement.appendChild(descel)
	var tw = descel.getComputedTextLength()
	var l = description.length
	while (tw + 20 > w && tw > 10) {
	    l = l - 1
	    descel.firstChild.replaceWholeText(description.substr(0, l) + "...")
	    tw = descel.getComputedTextLength()
	}
	uiItems.push(descel);
    }

    bgrectTitle = svgDocument.createElementNS(svgNS, "rect")
    bgrectTitle.setAttributeNS(null, "width", w)
    bgrectTitle.setAttributeNS(null, "height", 30)
    bgrectTitle.setAttributeNS(null, "fill", "#dddddd")
    bgrectTitle.setAttributeNS(null, "stroke", "black")
    svgDocument.rootElement.appendChild(bgrectTitle)
    uiItems.push(bgrectTitle);

    title1el = svgDocument.createElementNS(svgNS, "text")
    title1el.setAttributeNS(null, "font-weight", "bold")
    title1el.appendChild(svgDocument.createTextNode(name))
    svgDocument.rootElement.appendChild(title1el)
    uiItems.push(title1el);

    var cw = (w-20)/n
    var boxes = new Array(n)
    var fcs = new Array(n)
    for (var i=0; i<n; i++) {
	el = svgDocument.createElementNS(svgNS, "rect")
	el.setAttributeNS(null, "width", cw)
	el.setAttributeNS(null, "height", 30)
	el.setAttributeNS(null, "fill", colors[i])
	el.setAttributeNS(null, "stroke", "black")
	svgDocument.rootElement.appendChild(el)
	uiItems.push(el);
	boxes[i] = el
	elt = svgDocument.createElementNS(svgNS, "text")
	elt.appendChild(svgDocument.createTextNode(foldchanges[i]))
	elt.setAttributeNS(null, "font-family", "Arial")
	fcval = parseFloat(foldchanges[i])
	elt.setAttributeNS(null, "fill", "black")
	if (colors[i].charAt(0) == '#' && colors[i].length == 7) {
	    var c = colors[i].toLowerCase();
	    if (parseInt(c.substring(1,3),16)*299 + 
		parseInt(c.substring(3,5),16)*587 + 
		parseInt(c.substring(5,7),16)*114 < 125000) {
		// This background color appears quite dark, let's write the text in white
		elt.setAttributeNS(null, "fill", "white")
	    }
	}
	svgDocument.rootElement.appendChild(elt)
	uiItems.push(elt);
	fcs[i] = elt
    }

    // Compute box position
    x = evt.clientX + 9 + window.pageXOffset;
    y = evt.clientY + 17 + window.pageYOffset;
    if (x + w > ww) {
	// The tooltip is out of the SVG document (on the right), so display it on the left of the mouse cursor
	x = x - w;
    }
    if (y + h > wh) {
	// Same idea, when tooltip is below the document bottom
	y = y - h - 25;
    }

    // Put all elements on the right place
    bgrect.setAttributeNS(null, "x", x)
    bgrect.setAttributeNS(null, "y", y)
    bgrectTitle.setAttributeNS(null, "x", x)
    bgrectTitle.setAttributeNS(null, "y", y)
    title1el.setAttributeNS(null, "x", x + 10)
    title1el.setAttributeNS(null, "y", y + 20)
    if (description != null) {
	descel.setAttributeNS(null, "x", x + 10)
	descel.setAttributeNS(null, "y", y + 50)
    }
    for (var i=0; i<n; i++) {
	boxes[i].setAttributeNS(null, "y", y + 70)
	boxes[i].setAttributeNS(null, "x", x + 10 + cw*i)
	fcs[i].setAttributeNS(null, "y", y + 90)
	fcs[i].setAttributeNS(null, "x", x + 10 + cw*i + cw/2 - fcs[i].getComputedTextLength()/2)
    }
}

