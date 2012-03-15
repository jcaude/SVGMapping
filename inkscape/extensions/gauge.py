#!/usr/bin/env python

import math
import inkex
# simplestyle module provides functions for style parsing.
from simplestyle import *
from simpletransform import *

""" Gauge effect extension: Create a labelled Gauge shape """
class GaugeEffect(inkex.Effect):

    """ Constructor. Defines "--what" option of a script."""
    def __init__(self):
        # Call base class constructor.
        inkex.Effect.__init__(self)

         # Define options
        self.OptionParser.add_option('--label', action='store',
                                     type='string', dest='label',
                                     default='gauge',
                                     help='Shape Label?')
        self.OptionParser.add_option('--tab', action='store',
                                     type='string', dest='tab')
        self.OptionParser.add_option('--green', action='store',
                                     type='string', dest='green_zone',
                                     default='false',
                                     help='Green Zone?')
        self.OptionParser.add_option('--gstart', action='store',
                                     type='float', dest='green_start',
                                     default=0.3)
        self.OptionParser.add_option('--gstop', action='store',
                                     type='float', dest='green_stop',
                                     default=0.7)
        self.OptionParser.add_option('--orange', action='store',
                                     type='string', dest='orange_zone',
                                     default='false',
                                     help='Orange Zone?')
        self.OptionParser.add_option('--ostart', action='store',
                                     type='float', dest='orange_start',
                                     default=0.7)
        self.OptionParser.add_option('--ostop', action='store',
                                     type='float', dest='orange_stop',
                                     default=0.9)
        self.OptionParser.add_option('--red', action='store',
                                     type='string', dest='red_zone',
                                     default='false',
                                     help='Red Zone?')
        self.OptionParser.add_option('--rstart', action='store',
                                     type='float', dest='red_start',
                                     default=0.9)
        self.OptionParser.add_option('--rstop', action='store',
                                     type='float', dest='red_stop',
                                     default=1.0)

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

        # init.
        green = False
        orange = False
        red = False

        # retrieve label
        label = self.options.label
        if self.options.green_zone=='true':
            green=True
        if self.options.orange_zone=='true' :
            orange=True
        if self.options.red_zone=='true' :
            red=True
        green_start = self.options.green_start
        green_stop = self.options.green_stop
        orange_start = self.options.orange_start
        orange_stop = self.options.orange_stop
        red_start = self.options.red_start
        red_stop = self.options.red_stop
        
        # Get access to main SVG document element and get its dimensions.
        svg = self.document.getroot()
        width = inkex.unittouu(svg.get('width'))
        height = inkex.unittouu(svg.get('height'))

        # Create a new group for the shape
        group = inkex.etree.SubElement(svg,'g')

        # drawing parameters
        xorig = 366
        yorig = 495
        main_circle = 235
        inner_circle = 220
        white_circle=210

        # Arc 1 (grey background)
        style = {
            'fill' : '#a6b6b7',
            'fill-opacity' : '1',
            'stroke' : 'none'
            }
        self.addArc(group, style, cx=xorig, cy=yorig, rx=main_circle, ry=main_circle)

        # Arc 1.1 (green zone circle)
        if green:
            style['fill'] = '#008000'
            zstart = ((3+green_start*6)/8)*2*math.pi
            zstop = ((3+green_stop*6)/8)*2*math.pi
            self.addArc(group, style, cx=xorig, cy=yorig, rx=main_circle, ry=main_circle,
                        start=zstart, end=zstop)

        # Arc 1.1 (orange zone circle)
        if orange:
            style['fill'] = '#d48000'
            zstart = ((3+orange_start*6)/8)*2*math.pi
            zstop = ((3+orange_stop*6)/8)*2*math.pi
            self.addArc(group, style, cx=xorig, cy=yorig, rx=main_circle, ry=main_circle,
                        start=zstart, end=zstop)
            
        # Arc 1.1 (red zone circle)
        if red:
            style['fill'] = '#d40000'
            zstart = ((3+red_start*6)/8)*2*math.pi
            zstop = ((3+red_stop*6)/8)*2*math.pi
            self.addArc(group, style, cx=xorig, cy=yorig, rx=main_circle, ry=main_circle,
                        start=zstart, end=zstop)

        # Arc 2 (external circle)
        style = {
            'fill' : 'none',
            'stroke' : '#000000',
            'stroke-width' : '3',
            'stroke-miterlimit' : '4',
            'stroke-dasharray' : 'none'
            }
        self.addArc(group, style, cx=xorig, cy=yorig, rx=main_circle+5, ry=main_circle+5)

        # Arc 3 (secondary inner circle)
        style = {
            'fill' : '#c6d0d1',
            'fill-opacity' : '1',
            'stroke' : 'none'
            }
        self.addArc(group, style, cx=xorig, cy=yorig, rx=inner_circle, ry=inner_circle)

        # Arc 4 (white main content)
        style['fill'] = '#ffffff'
        self.addArc(group, style,
                    cx=xorig, cy=yorig, rx=white_circle, ry=white_circle)

        # Graduation
        style['fill'] = '#2e393a'
        # major
        for i in range(0,5) :
            angle = -135 + i*67.5
            self.addRect(group, style,
                         x=xorig, y=yorig, width=8, height=24,
                         transform='rotate(%f,%f,%f) matrix(1,0,0,1,-4,-210)' % (angle,xorig,yorig))
            # minor
            if(i < 4) :
                for j in range(1,5) :
                    angle2 = angle + j*13.5
                    self.addRect(group, style,
                                 x=xorig, y=yorig, width=4, height=12,
                                 transform='rotate(%f,%f,%f) matrix(1,0,0,1,-2,-210)' % (angle2,xorig,yorig))

        # Text
        style = {
            "font-size" : "36px",
            "font-style" : "normal",
            "font-weight" : "bold",
            "text-align" : "center",
            "line-height" : "125%",
            "letter-spacing" : "0px",
            "word-spacing" : "0px",
            "text-anchor" : "middle",
            "fill" : "#000000",
            "fill-opacity" : "1",
            "stroke" : "none",
            "display" : "inline",
            "font-family" : "Bitstream Vera Sans",
            "-inkscape-font-specification" : "Bitstream Vera Sans Bold"
            }
        self.addText(group, style,x=xorig, y=yorig-115, text="GAUGE", label=label+'.name')
        style['font-weight'] = 'normal'
        style['-inkscape-font-specification'] = 'Bitstream Vera Sans'
        self.addText(group, style,x=xorig, y=yorig+165, text="VALUE", label=label+'.value')
        style['font-size'] = '18px'
        style['text-align'] = 'start'
        style['text-anchor'] = 'start'
        delta = 120
        self.addText(group, style, x=xorig-delta, y=yorig+135, text="0", label=label+'.min')
        style['text-align'] = 'end'
        style['text-anchor'] = 'end'
        self.addText(group, style, x=xorig+delta, y=yorig+135, text="100", label=label+'.max')

       #  # Needle Sub-Group
        subgroup = inkex.etree.SubElement(group,'g')
        subgroup.set(inkex.addNS('label','inkscape'), label + '.needle')
        # !! rotation center values are not 'compatible'
        #    with inkscape but with 'batik' bbox calculations.. I don't figure why.
        subgroup.set(inkex.addNS('transform-center-x','inkscape'), '42.8')  
        subgroup.set(inkex.addNS('transform-center-y','inkscape'), '42.8') 
       # # subgroup.set('transform', 'matrix(0.45112806,4.5862154e-4,-4.5862154e-4,0.45112806,203.94172,9.709876)')
        
        # Path (needle main shape)
        style = {
            'fill' : '#cd3434',
            'fill-opacity' : '0.6',
            'stroke' : '#cd3333',
            'stroke-width' : '2.5',
            'stroke-miterlimit' : '4',
            'stroke-opacity' : '1',
            'stroke-dasharray' : 'none'
            }
        self.addPath(subgroup, style,
                     'm 242.52797,619.26107 c 0.92327,-6.44041 11.02829,-21.52192 13.95508,-24.45426 20.59591,-20.63501 70.49056,-60.85029 80.54883,-79.98793 20.59591,-20.635 22.43071,-23.18707 43.02661,-43.82207 2.92679,-2.93236 9.35314,-21.65035 20.3268,-11.15672 10.97367,10.49362 -7.03958,18.4832 -9.96638,21.41556 -20.5959,20.63499 -22.4307,23.18706 -43.02661,43.82206 -20.47908,8.93956 -59.95291,59.3529 -80.54882,79.9879 -2.92679,2.93235 -17.5366,13.55327 -24.31551,14.19546 z',
                     'cccszscsc')
        style = {
            'fill' : '#117ddd',
            'fill-opacity' : '1',
            'stroke' : '#5984a7',
            'stroke-width' : '1.5',
            'stroke-miterlimit' : '4',
            'stroke-opacity' : '1',
            'stroke-dasharray' : 'none'
            }
        self.addArc(subgroup, style, cx=xorig, cy=yorig, rx=19.604008, ry=19.604008)
                     
# Create effect instance and apply it
effect = GaugeEffect()
effect.affect()
