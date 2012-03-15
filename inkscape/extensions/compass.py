#!/usr/bin/env python

import math
import inkex
# simplestyle module provides functions for style parsing.
from simplestyle import *
from simpletransform import *

""" Compass effect extension: Create a labelled Compass shape """
class CompassEffect(inkex.Effect):

    """ Constructor. Defines "--what" option of a script."""
    def __init__(self):
        # Call base class constructor.
        inkex.Effect.__init__(self)

         # Define options
        self.OptionParser.add_option('--label', action='store',
                                     type='string', dest='label',
                                     default='gauge',
                                     help='Shape Label?')

    def checkGradient(self, gid):
        try:
            retval = self.document.xpath('//svg:linearGradient[@id="%s"]' % gid, namespaces=inkex.NSS)[0]
        except:
            return False
        return True

    def addLinearGradient(self, colors, gid):
        defs = self.xpathSingle('/svg:svg//svg:defs')
        if defs == None:
            defs = inkex.etree.SubElement(self.document.getroot(),inkex.addNS('defs','svg'))
        gradient = inkex.etree.SubElement(defs,inkex.addNS('linearGradient','svg'))
        gradient.set('id', gid)
        for (off,color) in colors.items():
            lg_stop = inkex.etree.Element(inkex.addNS('stop','svg'))
            lg_stop.set('style',formatStyle(color))
            lg_stop.set('offset', '%d' % off)
            gradient.append(lg_stop)
        defs.append(gradient)

    def addLinearGradientRef(self, href_id, gid, x1, y1, x2, y2, transform=None):
        defs = self.xpathSingle('/svg:svg//svg:defs')
        if defs == None:
            defs = inkex.etree.SubElement(self.document.getroot(),inkex.addNS('defs','svg'))
        gradient = inkex.etree.SubElement(defs,inkex.addNS('linearGradient','svg'))
        gradient.set(inkex.addNS('collect', 'inkscape'), 'always')
        gradient.set('id', gid)
        gradient.set(inkex.addNS('href','xlink'), '#%s' % href_id)
        gradient.set('x1', '%f' % x1)
        gradient.set('y1', '%f' % y1)
        gradient.set('x2', '%f' % x2)
        gradient.set('y2', '%f' % y2)
        gradient.set('gradientUnits', 'userSpaceOnUse')
        if not transform == None:
            gradient.set('gradientTransform', transform)
        defs.append(gradient)

    def addRadialGradientRef(self, href_id, gid, cx, cy, fx, fy, r, transform=None):
        defs = self.xpathSingle('/svg:svg//svg:defs')
        if defs == None:
            defs = inkex.etree.SubElement(self.document.getroot(),inkex.addNS('defs','svg'))
        gradient = inkex.etree.SubElement(defs,inkex.addNS('radialGradient','svg'))
        gradient.set(inkex.addNS('collect', 'inkscape'), 'always')
        gradient.set('id', gid)
        gradient.set(inkex.addNS('href','xlink'), '#%s' % href_id)
        gradient.set('cx', '%f' % cx)
        gradient.set('cy', '%f' % cy)
        gradient.set('fx', '%f' % fx)
        gradient.set('fy', '%f' % fy)
        gradient.set('r', '%f' % r)
        gradient.set('gradientUnits', 'userSpaceOnUse')
        if not transform == None:
            gradient.set('gradientTransform', transform)
        defs.append(gradient)

    def addPath(self, group, style, d, nodetypes=None, label=None, transform=None):
        path = inkex.etree.Element(inkex.addNS('path','svg'))
        path.set('style', formatStyle(style))
        path.set('d', d)
        if not label==None:
            path.set(inkex.addNS('label','inkscape'),label)
        if not nodetypes==None:
            path.set(inkex.addNS('nodetypes', 'sodipodi'), nodetypes)
        if not transform==None:
            path.set('transform', transform)
        group.append(path)

    def addArc(self, group, style, cx, cy, rx, ry, d=None, start=None, end=None, open=None, transform=None):
        path = inkex.etree.Element(inkex.addNS('path','svg'))
        path.set('style', formatStyle(style))
        path.set(inkex.addNS('type', 'sodipodi'), 'arc')
        path.set(inkex.addNS('cx', 'sodipodi'), '%f' % cx)
        path.set(inkex.addNS('cy', 'sodipodi'), '%f' % cy)
        path.set(inkex.addNS('rx', 'sodipodi'), '%f' % rx)
        path.set(inkex.addNS('ry', 'sodipodi'), '%f' % ry)
        if d==None:
            if start==None:
                xs = cx + rx
                ys = cy
                xe = - 2*rx
                ye = 0
                dvalue='m %f,%f a %f,%f 0 1 1 %f,%f' % (xs,ys,rx,ry,xe,ye)
                xe = rx
                ye = ry
                dvalue = dvalue + ' %f,%f 0 1 1' % (xe,ye)
                xe = 2*rx
                ye = 0
                dvalue = dvalue + ' %f,%f z' % (xe,ye)
            else:
                xs = cx + rx * math.cos(start)
                ys = cy + ry * math.sin(start)
                xe = rx * (math.cos(end) - math.cos(start))
                ye = ry * (math.sin(end) - math.sin(start))
                dvalue='m %f,%f a %f,%f 0 0 1 %f,%f' % (xs,ys,rx,ry,xe,ye)
                if not open==None:
                    xc = - rx * math.cos(end)
                    yc = - ry * math.sin(end)
                    dvalue = dvalue + ' l %f,%f z' % (xc,yc)
        else:
            dvalue=d
        path.set('d', dvalue)
        if not start==None:
            path.set(inkex.addNS('start','sodipodi'), '%f' % start)
            path.set(inkex.addNS('end','sodipodi'), '%f' % end)
        if not open==None:
            path.set(inkex.addNS('open','sodipodi'), open)
        if not transform==None:
            path.set('transform', transform)
        group.append(path)

    def addLine(self, group, style, x1, y1, x2, y2, transform=None):
        line = inkex.etree.Element(inkex.addNS('line','svg'))
        line.set('style', formatStyle(style))
        line.set('x1', '%f' % x1)
        line.set('y1', '%f' % y1)
        line.set('x2', '%f' % x2)
        line.set('y2', '%f' % y2)
        if not transform==None:
            line.set('transform', transform)
        group.append(line)

    def addRect(self, group, style, x, y, width, height, ry=0, transform=None):
        rect = inkex.etree.Element(inkex.addNS('rect','svg'))
        rect.set('style', formatStyle(style))
        rect.set('x', '%f' % x)
        rect.set('y', '%f' % y)
        rect.set('width', '%f' % width)
        rect.set('height', '%f' % height)
        rect.set('ry', '%f' % ry)
        if not transform==None:
            rect.set('transform', transform)
        group.append(rect)

    def addText(self, group, style, x, y, text, label=None, transform=None):
        etext = inkex.etree.Element(inkex.addNS('text','svg'))
        etext.set('style', formatStyle(style))
        etext.set('x', '%f' % x)
        etext.set('y', '%f' % y)
        etext.set(inkex.addNS('linespacing', 'sodipodi'), '125%')
        etext.text = text
        if not label==None:
            etext.set(inkex.addNS('label','inkscape'), label)
        if not transform==None:
            etext.set('transform', transform)
        group.append(etext)

    """ Effect behaviour. Oberrides base class' method. """
    def effect(self):

        # retrieve label
        label = self.options.label
        
        # Get access to main SVG document element and get its dimensions.
        svg = self.document.getroot()
        width = inkex.unittouu(svg.get('width'))
        height = inkex.unittouu(svg.get('height'))

        # Create a main group for the whole shape
        main = inkex.etree.SubElement(svg,'g')

        # Create a new group for the shape background
        group = inkex.etree.SubElement(main,'g')

        # drawing parameters
        xorig = 366
        yorig = 495
        main_circle = 235
        inner_circle = 215
        white_circle=210
        inner2_circle = 105

        # Arc 1 (grey background)
        style = {
            'fill' : '#b6b6b6',
            'stroke' : '#000000',
            'fill-opacity' : '1',
            'stroke-width' : '2'
            }
        self.addArc(group, style, cx=xorig, cy=yorig, rx=main_circle, ry=main_circle)

        # Arc 2 (external circle)
        style = {
            'fill' : 'none',
            'stroke' : '#000000',
            'stroke-width' : '2',
            'stroke-miterlimit' : '4',
            'stroke-dasharray' : 'none'
            }
        self.addArc(group, style, cx=xorig, cy=yorig, rx=main_circle+8, ry=main_circle+8)

        # Graduations (contour decorations)
        style = {
            'fill' : '#000000',
            'fill-opacity' : '1',
            'stroke' : 'none'
            }
        for i in range(0,90) :
            angle = i * 360/90
            t='rotate(%f,%f,%f) matrix(1,0,0,1,-2,-%f)' % (angle,xorig,yorig,main_circle+8)
            self.addRect(group, style, x=xorig, y=yorig, width=2, height=8, transform=t)

        style = {
            "font-size" : "12px",
            "font-style" : "normal",
            "font-weight" : "normal",
            "text-align" : "center",
            "line-height" : "125%",
            "letter-spacing" : "0px",
            "word-spacing" : "0px",
            "text-anchor" : "middle",
            "fill" : "#646464",
            "fill-opacity" : "1",
            "stroke" : "none",
            "display" : "inline",
            "font-family" : "Bitstream Vera Sans",
            "-inkscape-font-specification" : "Bitstream Vera Sans"
            }
        for angle in range(0, 360, 10) :
            t='rotate(%f,%f,%f) matrix(1,0,0,1,0,-%f)' % (angle,xorig,yorig,inner_circle+4)
            self.addText(group, style,x=xorig, y=yorig, text="%d" % (angle), transform=t)

        # Arc 3 (secondary inner circle)
        style = {
            'fill' : '#d0d0d0',
            'fill-opacity' : '1',
            'stroke' : 'none'
            }
        self.addArc(group, style, cx=xorig, cy=yorig, rx=inner_circle, ry=inner_circle)

        # Arc 4 (white main content)
        style['fill'] = '#ffffff'
        self.addArc(group, style,
                    cx=xorig, cy=yorig, rx=white_circle, ry=white_circle)

        # Path (quadrants)
        style = {
            'stroke' : '#d0d0d0',
            'fill-opacity' : '1',
            'stroke-width' : '1px'
            }
        for i in range (0,4):
            angle = i*(360/8)+360/16
            self.addLine(group, style,
                         x1=xorig, y1=yorig-inner_circle,
                         x2=xorig, y2=yorig+inner_circle,
                         transform='rotate(%f,%f,%f)' % (angle,xorig,yorig))
            
        # Arc 5 (secondary circle)
        style = {
            'stroke' : '#d0d0d0',
            'stroke-opacity' : '1',
            'fill' : 'none'
            }
        self.addArc(group, style, cx=xorig, cy=yorig, rx=inner2_circle, ry=inner2_circle)

        # Arc 6 (compass rose)
        style = {
            'fill' : '#afdde9',
            'fill-opacity' : '1',
            'stroke' : 'none'
            }
        for angle in range(45, 360, 90):
            triangle = 'm %f %f ' % (xorig-10, yorig)
            triangle = triangle + 'l %f %f' % (20,0)
            triangle = triangle + 'l %f %f z' % (-10,inner2_circle)
            self.addPath(group,style, d=triangle,
                         transform='rotate(%f,%f,%f)' % (angle,xorig,yorig))
        for angle in range(0, 360, 90):
            triangle = 'm %f %f ' % (xorig-10, yorig)
            triangle = triangle + 'l %f %f' % (20,0)
            triangle = triangle + 'l %f %f z' % (-10,inner2_circle + (inner_circle-inner2_circle)/2)
            self.addPath(group,style, d=triangle,
                         transform='rotate(%f,%f,%f)' % (angle,xorig,yorig))

        # Text (cardinal points)
        style = {
            "font-size" : "32px",
            "font-style" : "normal",
            "font-weight" : "normal",
            "text-align" : "center",
            "line-height" : "125%",
            "letter-spacing" : "0px",
            "word-spacing" : "0px",
            "text-anchor" : "middle",
            "fill" : "#000000",
            "fill-opacity" : "1",
            "stroke" : "none",
            "display" : "inline",
            "font-family" : "Garamond",
            "-inkscape-font-specification" : "Garamond"
            }
        t='rotate(%s,%f,%f) matrix(1,0,0,1,0,-%f)' % ("%f",xorig,yorig,inner2_circle+1)
        self.addText(group, style, x=xorig, y=yorig, text="NE", transform=t % (45))
        self.addText(group, style, x=xorig, y=yorig, text="SE", transform=t % (135))
        self.addText(group, style, x=xorig, y=yorig, text="SW", transform=t % (225))
        self.addText(group, style, x=xorig, y=yorig, text="NW", transform=t % (315))
        style['font-size'] = '72px'
        t='rotate(%s,%f,%f) matrix(1,0,0,1,0,-%f)' % ("%f",xorig,yorig,inner2_circle+25)
        self.addText(group, style, x=xorig, y=yorig, text="N", transform=t % (0))
        self.addText(group, style, x=xorig, y=yorig, text="E", transform=t % (90))
        self.addText(group, style, x=xorig, y=yorig, text="S", transform=t % (180))
        self.addText(group, style, x=xorig, y=yorig, text="W", transform=t % (270))
        
        # Needle Sub-Group
        subgroup = inkex.etree.SubElement(main,'g')
        subgroup.set(inkex.addNS('label','inkscape'), label + '.needle')

        # needle top section
        delta = 7
        needle_len = main_circle - delta
        needle_circle = 20
        style = {
            'fill' : '#cc0000',
            'fill-opacity' : '1',
            'stroke' : '#a40000',
            'stroke-width': '1.4px',
            'stroke-linecap' : 'round',
            'stroke-linejoin' : 'round',
            'stroke-miterlimit' : '4',
            'stroke-opacity' : '1',
            'stroke-dasharray' : 'none',
            'stroke-dashoffset' : '0'
            }
        sec1 = 'm %f,%f %f,%f ' % (xorig, yorig-needle_len, -delta, needle_len-needle_circle)
        sec1 = sec1 + 'c %f,%f %f,%f %f,%f ' % (-(needle_circle-delta)/2,0,-(needle_circle-delta),(needle_circle-delta)/2,-(needle_circle-delta),needle_circle)
        sec1 = sec1 + 'l %f,%f ' % (2*needle_circle, 0)
        sec1 = sec1 + 'c %f,%f %f,%f %f,%f ' % (0,-(needle_circle-delta)/2-delta,-(needle_circle-delta)/2,-needle_circle,-(needle_circle-delta),-needle_circle)
        sec1 = sec1 + 'l %f,%f z' % (-delta, -(needle_len-needle_circle))
        self.addPath(subgroup, style, d=sec1)

        # bottom section
        style['fill'] = '#3465a4'
        style['stroke'] = '#204a87'
        self.addPath(subgroup, style, d=sec1, transform='rotate(180,%f,%f)' % (xorig,yorig))

        # center circle
        style['fill'] = '#b6b6b6'
        style['stroke'] = '#d0d0d0'
        style['stroke-width'] = '1px'
        self.addArc(subgroup, style, cx=xorig, cy=yorig, rx=needle_circle-delta, ry=needle_circle-delta)

        # decoration lines
        style['fill'] = 'none'
        r = needle_circle-delta-3
        x1 = xorig + r*math.cos(math.pi/4)
        y1 = yorig + r*math.sin(math.pi/4)
        x2 = xorig + r*math.cos(math.pi/4 + math.pi)
        y2 = yorig + r*math.sin(math.pi/4 + math.pi)
        self.addLine(subgroup, style, x1=x1, y1=y1, x2=x2, y2=y2)
        x1 = xorig + r*math.cos(-math.pi/4)
        y1 = yorig + r*math.sin(-math.pi/4)
        x2 = xorig + r*math.cos(-(math.pi/4 + math.pi))
        y2 = yorig + r*math.sin(-(math.pi/4 + math.pi))
        self.addLine(subgroup, style, x1=x1, y1=y1, x2=x2, y2=y2)
                     
# Create effect instance and apply it
effect = CompassEffect()
effect.affect()
