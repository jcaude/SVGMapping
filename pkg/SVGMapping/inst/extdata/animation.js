// Animation engine
// --------------------------------------------------

// Find the element whatever the form of url:
// can be: id, #id, url(#id)
function findElementByURL(url) {
	if (url == null) return null
	var element = svgDocument.getElementById(url)
	if (element != null) return element
	if (url.substring(0,3) == "url") {
		var k = url.indexOf("#")
		if (k >= 0) {
			var s = url.substring(k+1)
			k = s.indexOf("\"")
			if (k < 0) {
				k = s.indexOf(")")
			}
			if (k >= 0) {
				s = s.substring(0, k)
				element = svgDocument.getElementById(s)
				if (element != null) return element
			}
		}
	}
	if (url.charAt(0) == '#') {
		element = svgDocument.getElementById(url.substring(1))
		if (element != null) return element
	}
	return(null)
}

var AnimationType = {
	None:0,
	PartialFill:1,
	Pie:2,
	Stripes:3
}

var currentAnimation = AnimationType.None

// partial-fill animation
var offset1 = null
var offset2 = null
var stop1 = null
var stop2 = null
var deltaOffset = 1
var partialFillDeltaT = 10

// pie animation
var pieParts = null
var currentPart = null
var pieDeltaT = null

// stripes animation
var currentStripe = null
var stripes = null
var originalStopStyles = null
var stripesDeltaT = null

function changeStopOffset(stopNode, newOffset) {
	stopNode.setAttributeNS(null, "offset", newOffset)
}

function stopAnimation() {
	if (currentAnimation == AnimationType.PartialFill) {
		// end animation: restore original values
		changeStopOffset(stop1, offset1)
		changeStopOffset(stop2, offset2)
	} else if (currentAnimation == AnimationType.Pie) {
		for (var i=0; i<pieParts.length; i++) {
			pieParts[i].setAttributeNS(null, "visibility", "visible")
		}
		pieParts = null
		currentPart = null
		pieDeltaT = null
	}  else if (currentAnimation == AnimationType.Stripes) {
		for (var i=0; i<stripes.length; i++) {
			stripes[i].setAttributeNS(null, "style", originalStopStyles[i])
		}
		currentStripe = null
		stripes = null
		originalStopStyles = null
		stripesDeltaT = null
	}
	currentAnimation = AnimationType.None
}

function nextAnimationStep() {
	if (currentAnimation == AnimationType.PartialFill) {
		var currentOffset = stop1.offset.baseVal
		currentOffset += deltaOffset
		if (currentOffset > offset1) {
			stopAnimation()
		} else {
			changeStopOffset(stop1, currentOffset)
			changeStopOffset(stop2, currentOffset)
			setTimeout("nextAnimationStep()", partialFillDeltaT)
		}
	} else if (currentAnimation == AnimationType.Pie) {
		if (currentPart < pieParts.length - 1) {
			pieParts[currentPart].setAttributeNS(null, "visibility", "visible")
			currentPart++
			setTimeout("nextAnimationStep()", pieDeltaT)
		} else {
			stopAnimation()
		}
	} else if (currentAnimation == AnimationType.Stripes) {
		if (currentStripe < stripes.length - 1) {
			stripes[currentStripe].setAttributeNS(null, "style", originalStopStyles[currentStripe])
			stripes[currentStripe+1].setAttributeNS(null, "style", originalStopStyles[currentStripe+1])
			currentStripe += 2
			setTimeout("nextAnimationStep()", stripesDeltaT)
		} else {
			stopAnimation()
		}
	}
}

function animateStripes(evt) {
	// stop any already running animation
	stopAnimation()
	var element = evt.target
	var gradientId = element.style.fill
	if (gradientId == null) return
	var gradient = findElementByURL(element.style.fill)
	if (gradient == null) return
	
	stripes = new Array()
	for (var i=0; i<gradient.childNodes.length; i++) {
		var child = gradient.childNodes[i]
		if (child.tagName == "stop") {
			stripes.push(child)
		}
	}
	if (stripes.length < 2) {
		// only 1 stripe, don't run animation
		stripes = null
		return
	}
	// we are decided to do run animation
	currentAnimation = AnimationType.Stripes
	// hide all parts
	originalStopStyles = new Array()
	for (var i=0; i<stripes.length; i++) {
		originalStopStyles.push(stripes[i].getAttributeNS(null, "style"))
		stripes[i].setAttributeNS(null, "style", "stop-opacity:0")
	}
	currentStripe = 0
	stripesDeltaT = 500/(stripes.length/2)
	setTimeout("nextAnimationStep()", stripesDeltaT)
}

function animatePie(evt) {
	// stop any already running animation
	stopAnimation()
	var element = evt.target
	var g = element.parentNode
	if (g.tagName == "a") {
		// we have a link, so go up again
		g = g.parentNode
	}
	var g2 = null
	for (var i=0; i<g.childNodes.length; i++) {
		var child = g.childNodes[i]
		if (child.tagName == "g") {
			g2 = child
			break
		}
	}
	if (g2 == null) return
	// g2 is a <g> element containing the pie parts
	pieParts = new Array()
	for (var i=0; i<g2.childNodes.length; i++) {
		var child = g2.childNodes[i]
		if (child.tagName == "path") {
			pieParts.push(child)
		}
	}
	if (pieParts.length < 2) {
		// pie has only one section, don't run animation
		pieParts = null
		return
	}
	// we are decided to do run animation
	currentAnimation = AnimationType.Pie
	// hide all parts
	for (var i=0; i<pieParts.length; i++) {
		pieParts[i].setAttributeNS(null, "visibility", "hidden")
	}
	currentPart = 0
	pieDeltaT = 500/pieParts.length
	setTimeout("nextAnimationStep()", pieDeltaT)
}

function animatePartialFill(evt) {
	// stop any already running animation
	stopAnimation()
	var element = evt.target
	
	// find mask
	if (!element.hasAttributeNS(null, "mask")) return
	var mask = findElementByURL(element.getAttributeNS(null, "mask"))
	if (mask == null) return
	
	// find mask path
	var maskChild = null
	for (var i=0; i<mask.childNodes.length; i++) {
		var child = mask.childNodes[i]
		if (child instanceof SVGElement) {
			maskChild = child
			break
		}
	}
	if (maskChild == null) return
	
	// find mask gradient
	var gradientId = maskChild.style.fill
	if (gradientId == null) return
	var gradient = findElementByURL(maskChild.style.fill)
	if (gradient == null) return
	var k = 1
	for (var i=0; i<gradient.childNodes.length; i++) {
		var child = gradient.childNodes[i]
		if (child.tagName == "stop") {
			if (k == 2) {
				stop1 = child
			} else if (k == 3) {
				stop2 = child
			}
			k++
		}
	}
	currentAnimation = AnimationType.PartialFill
	// save real values
	offset1 = stop1.offset.baseVal
	offset2 = stop2.offset.baseVal
	changeStopOffset(stop1, 0)
	changeStopOffset(stop2, 0)
	deltaOffset = offset1 / 20
	if (deltaOffset == 0)
		deltaOffset = 0.01
	setTimeout("nextAnimationStep()", partialFillDeltaT)
}
