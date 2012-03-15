#!/usr/bin/env python

import math
import inkex
# simplestyle module provides functions for style parsing.
from simplestyle import *
from simpletransform import *

""" Tube effect extension: Create a labelled Tube shape """
class TubeEffect(inkex.Effect):

    """ Constructor. Defines "--what" option of a script."""
    def __init__(self):
        # Call base class constructor.
        inkex.Effect.__init__(self)

         # Define options
        self.OptionParser.add_option('--label', action='store',
                                     type='string', dest='label',
                                     default='gauge',
                                     help='Shape Label?')
        self.OptionParser.add_option('--fillcolor', action='store',
                                     type='int', dest='fill_color',
                                     default='100',
                                     help='Filling Color?')

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
            lg_stop.set('offset', '%f' % (float(off)/100))
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

    def addRect(self, group, style, x, y, width, height, rx=0, ry=0, transform=None):
        rect = inkex.etree.Element(inkex.addNS('rect','svg'))
        rect.set('style', formatStyle(style))
        rect.set('x', '%f' % x)
        rect.set('y', '%f' % y)
        rect.set('width', '%f' % width)
        rect.set('height', '%f' % height)
        rect.set('rx', '%f' % rx)
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

    """ TSL to RGB conversion, taken from http://fr.wikipedia.org/wiki/Codage_informatique_des_couleurs. """
    def TSL2RGB(self,T,S,L):
	T = float(T*240/255)
	L = float(L*240/255)
	S = float(S*240/255)
        T0 = 6*T/240
        if (L < 120):
            M = 255 * (L/240) * (1 + S/240)
            m = 255 * (L/240) * (1 - S/240)
	else:
            M = 255 * ( (L/240) * (1-S/240) + (S/240) )
            m = 255 * ( (L/240) * (1+S/240) - (S/240) )
        delta = M - m
        i = int(T0)
        if(i==0):
		R = M
		G = m + T0*delta
		B = m
        elif(i==1):
		R = m + (2-T0)*delta
		G = M
		B = m
	elif(i==2):
		R = m
		G = M
		B = m + (T0-2)*delta
        elif(i==3):
		R = m
		G = m + (4-T0)*delta
		B = M
        elif(i==4):
		R = m + (T0-4)*delta
		G = m
		B = M
        elif(i>=5):
		R = M
		G = m
		B = m + (6-T0)*delta
	R = max(0,min(255,int(round(R))))
	G = max(0,min(255,int(round(G))))
	B = max(0,min(255,int(round(B))))
	return ('#%02x%02x%02x' % (R,G,B))
	
            
    """ Effect behaviour. Oberrides base class' method. """
    def effect(self):

        # retrieve label
        label = self.options.label
        fill_color = self.options.fill_color

        # compute fill gradient colors
        c001_1 = self.TSL2RGB(fill_color,255,225)
        c001_2 = self.TSL2RGB(fill_color,158,118)
        c001_3 = self.TSL2RGB(fill_color,240,123)

        # color constants ('#fdffc2, #babf2d, #e5ef0)
        color001_1 = { 'stop-color' : c001_1, 'stop-opacity' : '0.58333331' }
        color001_2 = { 'stop-color' : c001_2, 'stop-opacity' : '0.32291666' }
        color001_3 = { 'stop-color' : c001_3, 'stop-opacity' : '0' }
        color002_1 = { 'stop-color' : '#e5ffff', 'stop-opacity' : '1' }
        color002_2 = { 'stop-color' : '#b0e0ff', 'stop-opacity' : '0.49803922' }
        color002_3 = { 'stop-color' : '#b0e0ff', 'stop-opacity' : '0' }
        color003_1 = { 'stop-color' : '#ffffff', 'stop-opacity' : '0.57291669' }
        color003_2 = { 'stop-color' : '#ffffff', 'stop-opacity' : '0' }

        # Get access to main SVG document element and get its dimensions.
        svg = self.document.getroot()
        width = inkex.unittouu(svg.get('width'))
        height = inkex.unittouu(svg.get('height'))

        # Create a new group for the shape
        group = inkex.etree.SubElement(svg,'g')

        # drawing parameters
        xorig = 200
        yorig = 200
        tube_len = 400

        # init. color gradients
        if not self.checkGradient('TULinearGradient002'):
            self.addLinearGradient({0:color002_1, 30:color002_2, 100:color002_3},
                                   'TULinearGradient002')
            self.addLinearGradientRef('TULinearGradient002', 'TULinearGradient101',
                                      x1=186.52565, y1=394.20596, x2=255.66185, y2=394.20596,
                                      transform='translate(44.269347,102.12162)')
            self.addLinearGradient({0:color003_1, 100:color003_2},
                                   'TULinearGradient003')
            self.addLinearGradientRef('TULinearGradient003', 'TULinearGradient103',
                                      x1=197.61035, y1=398.0148, x2=198.11037, y2=452.05017,
                                      transform='matrix(1.2196681,0,0,1.1384425,-7.22545,31.254535)')
            self.addLinearGradientRef('TULinearGradient003', 'TULinearGradient104',
                                      x1=190.60016, y1=398.4248, x2=217.39427, y2=397.24161,
                                      transform='matrix(1.3562702,0,0,0.17557903,-356.44513,661.90356)')
        if not self.checkGradient('TULinearGradient100' + label):
            self.addLinearGradient({0:color001_1, 42:color001_2, 100:color001_3},
                                   'TULinearGradient001' + label)
            self.addLinearGradientRef('TULinearGradient001' + label,
                                      'TULinearGradient100' + label,
                                      x1=192.12421, y1=493.87622, x2=250.06329, y2=493.87622,
                                      transform='translate(44.270703,102.36788)')

        # Path 1 (tube main shape) LR:linearGradient22976
        style={
            'fill' : 'url(#TULinearGradient101)',
            'fill-opacity': '1',
            'fill-rule' : 'nonzero',
            'stroke' : '#000000',
            'stroke-linecap' : 'butt',
            'stroke-opacity': '1'
            }
        self.addPath(group, style,
                     d='m 231.30067,286.59032 0,387.03125 c -0.19407,24.17773 21.59102,34.32959 35.6874,34.62485 18.26323,-0.13392 32.73977,-15.12842 32.4375,-34.625 l 0,-387.03125 c -0.22928,-1.5179 -1.93845,-2.0462 -3.68745,-2.2187 l -60.75,0 c -1.9942,0.19873 -3.42482,0.49132 -3.68745,2.21885 z',
                     nodetypes='cccccccc')

        # Path 2 (tube content outline)
        style={
            'fill' : 'none',
            'stroke' : '#000000',
            'stroke-linecap' : 'butt',
            'stroke-opacity' : '1'
            }
        self.addPath(group,style,
                     d='m 236.89618,288.28677 c 0,63.5792 0,332.25185 0,386.66397 2.50051,19.87897 17.26485,26.6041 28.60067,26.6041 11.33582,0 26.38697,-7.67947 28.33658,-26.6041 l 0,-386.66397',
                     nodetypes='cczcc')

        # Path 3 (tube content) LR:linearGradient22979
        style={
            'fill' : 'url(#TULinearGradient100'+label+')',
            'fill-opacity' : '1',
            'fill-rule' : 'nonzero',
            'stroke' : 'none'
            }
        self.addPath(group,style,
                     d='m 236.89618,288.50356 c 0,63.5792 0,332.03506 0,386.44718 2.50051,19.87897 17.26485,26.6041 28.60067,26.6041 11.33582,0 26.38697,-7.67947 28.33658,-26.6041 l 0,-386.44715 c -0.51136,-1.58932 -56.44534,-1.58932 -56.93725,-3e-5 z',
                     nodetypes='cczccc', label=label)

        # Rect 1 (bottom shadow) LR:linearGradient22973
        style={
            'fill' : 'url(#TULinearGradient103)',
            'fill-opacity' : '1',
            'fill-rule' : 'nonzero',
            'stroke' : 'none'
            }
        self.addRect(group, style,
                     x=234.4035, y=285.9, width=15.464247, height=345,
                     rx=6.5242753, ry=8.2769442)

        # Rect 2 (left shadow) LR:linearGradient22970
        style['fill'] = 'url(#TULinearGradient104)'
        self.addRect(group,style,
                     x=-87.753525, y=701.41852, width=33.507381, height=47.962883,
                     rx=16.753691, ry=23.981441,
                     transform='matrix(0.91779787,-0.39704794,0.44239192,0.89682183,0,0)')
        

# Create effect instance and apply it
effect = TubeEffect()
effect.affect()
