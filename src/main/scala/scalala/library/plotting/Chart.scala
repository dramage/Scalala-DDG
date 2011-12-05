/*
 * Distributed as part of Scalala-DDG.
 *
 * Copyright (C) 2011- Daniel Ramage
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.

 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.

 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110 USA
 */
package scalala;
package library;
package plotting;

import java.awt.{Graphics2D,Shape,RenderingHints};
import java.awt.{Paint,Color,Stroke,BasicStroke,Font};
import java.awt.font.{TextLayout};
import java.awt.geom.{AffineTransform,Point2D,Line2D,Ellipse2D,Rectangle2D,Arc2D,Path2D,GeneralPath};
import javax.swing.{JFrame,JScrollPane,WindowConstants};
import java.io.File;

/**
 * Represents an optional value from D to V.  Whenever an Attribute is
 * required, you may either provide a literal value of V, a function
 * from D to V, or a partial function (such as from a case statement
 * or a scala Map) from D to V.  These conversions are provided by the
 * Attribute companion object and do not need to be imported to be in
 * scope.
 *
 * @author dramage 
 */
trait Attribute[-D,+V] extends PartialFunction[D,V];

/**
 * Companion object for Attribute providing implicit conversions from
 * constants, functions, and partial functions.
 *
 * @author dramage
 */
object Attribute {
  implicit def constant[V](const : =>V) : Attribute[Any,V]
  = new Attribute[Any,V] {
    override def isDefinedAt(item : Any) = true;
    override def apply(item : Any) = const;
  }
  
  implicit def function[D,V](f : (D=>V)) : Attribute[D,V]
  = new Attribute[D,V] {
    override def isDefinedAt(item : D) = true;
    override def apply(item : D) = f(item);
  }
  
  implicit def partial[D,V](f : PartialFunction[D,V]) : Attribute[D,V]
  = new Attribute[D,V] {
    override def isDefinedAt(item : D) = f.isDefinedAt(item);
    override def apply(item : D) = f(item);
  }
}

class Chart {
  val plotters = new scala.collection.mutable.ArrayBuffer[Plotter]();

  private var _width : ()=>Int = ()=>600;
  def width : Int = _width();
  def width_=(w : =>Int) = _width = ()=>w;
  
  private var _height : ()=>Int = ()=>600;
  def height : Int = _height();
  def height_=(h : =>Int) = _height = ()=>h;

  def show(title : String = "Scalala", panel : ChartPanel = new ChartPanel(this, width, height)) : JFrame = {
    val frame = new JFrame(title);
    frame.add(new JScrollPane(panel));
    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
    frame.pack();
    frame.setVisible(true);
    frame;
  }
  
  def saveas(path : File, dpi : Int = 72) : Unit =
    ExportGraphics.writeFile(path, this.render, width=width, height=height, dpi=dpi);
  
  def add(plotters : Plotter*) : Chart =
    this.addAll(plotters);
  
  def addAll(plotters : Traversable[Plotter]) : Chart = {
    this.plotters ++= plotters;
    this;
  }
  
  def render(g : Graphics2D) {
    println(g.getClass);
  
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                       RenderingHints.VALUE_ANTIALIAS_ON);
    g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING,
                       RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
    
		for (plotter <- plotters) {
		  plotter.render(g);
		}
  }
}

object Chart {
  def apply(width : =>Int, height : =>Int) = {
    var chart = new Chart();
    chart.width = width;
    chart.height = height;
    chart;
  }
}

trait Plotter {
  def render(g : Graphics2D);
}

object Plotter {
  val defaultLinePaint = Color.DARK_GRAY;
  val defaultFillPaint = Color.LIGHT_GRAY;
  val defaultStroke = new BasicStroke(1,BasicStroke.CAP_ROUND,BasicStroke.JOIN_ROUND);
  val defaultLabelFont = new Font(Font.SANS_SERIF, Font.PLAIN, 12);
  val defaultLabelColor = Color.BLACK;
  val noStroke = new BasicStroke(0);
}

/**
 * A vertical line drawn with a particular stroke.
 *
 * @author dramage
 */
case class VRule(left : Double, top : Double, height : Double,
  linePaint : Paint = Plotter.defaultLinePaint,
  lineStroke : Stroke = Plotter.defaultStroke)
extends Plotter {
  val line2d = new Line2D.Double(left, top, left, top + height)
  def render(g : Graphics2D) = {
    val originalPaint  = g.getPaint;
    val originalStroke = g.getStroke;
    g.setPaint(linePaint);
    g.setStroke(lineStroke);
    g.draw(line2d);
    g.setPaint(originalPaint);
    g.setStroke(originalStroke);
  }
}

/**
 * A horizontal line drawn with a particular stroke.
 *
 * @author dramage
 */
case class HRule(left : Double, top : Double, width : Double,
  linePaint : Paint = Plotter.defaultLinePaint,
  lineStroke : Stroke = Plotter.defaultStroke)
extends Plotter {
  val line2d = new Line2D.Double(left, top, left + width, top)
  def render(g : Graphics2D) = {
    val originalPaint  = g.getPaint;
    val originalStroke = g.getStroke;
    g.setPaint(linePaint);
    g.setStroke(lineStroke);
    g.draw(line2d);
    g.setPaint(originalPaint);
    g.setStroke(originalStroke);
  }
}


case class Dots[D](
  data : Iterable[D],
  x : D=>Double, y : D=>Double,
  r : Attribute[D,Double],
  linePaint : Attribute[D,Paint] = Plotter.defaultLinePaint,
  lineStroke : Attribute[D,Stroke] = Plotter.defaultStroke,
  fillPaint : Attribute[D,Paint] = Plotter.defaultFillPaint
) extends Plotter {
  def render(g : Graphics2D) = {
    val shape = new Ellipse2D.Double();
    
    val originalPaint = g.getPaint;
    val originalStroke = g.getStroke;
    for (item <- data) {
      val ir = r(item);
      val ix = x(item) - ir;
      val iy = y(item) - ir;
      val id = 2 * ir;

      shape.setFrame(ix,iy,id,id);
      
      g.setStroke(lineStroke(item));
      g.setPaint(fillPaint(item));
      g.fill(shape);
      g.setPaint(linePaint(item));
      g.draw(shape);
    }
    g.setPaint(originalPaint);
    g.setStroke(originalStroke);
  }

  /** Returns the displayed positions of all data items with respect to the requested anchor. */
  def anchors(anchor : Anchor) : Iterable[(Double,Double)] = {
    for (item <- data) yield {
      var ix = x(item);
      var iy = y(item);
      val ir = r(item);
      if (anchor.isTop) {
        iy -= ir;
      } else if (anchor.isBottom) {
        iy += ir;
      }
      
      if (anchor.isLeft) {
        ix -= ir;
      } else if (anchor.isBottom) {
        ix += ir;
      }
      
      (ix,iy);
    }
  }
}

case class Bars[D](
  data : Iterable[D],
  x : D=>Double, y : D=>Double,
  width : Attribute[D,Double],
  height : Attribute[D,Double],
  anchor : Attribute[D,Anchor],
  linePaint : Attribute[D,Paint] = Plotter.defaultLinePaint,
  lineStroke  : Attribute[D,Stroke] = Plotter.defaultStroke,
  fillPaint : Attribute[D,Paint] = Plotter.defaultFillPaint
) extends Plotter {
  def render(g : Graphics2D) = {
    val shape = new Rectangle2D.Double();
    
    val originalPaint = g.getPaint;
    val originalStroke = g.getStroke;
    for (item <- data) {
      var ix = x(item);
      var iy = y(item);
      val iwidth = width(item);
      val iheight = height(item);
      val ianchor = anchor(item);
      
      if (ianchor.isCenter) {
        ix -= iwidth / 2.0;
      } else if (ianchor.isRight) {
        ix -= iwidth;
      }
      
      if (ianchor.isMiddle) {
        iy -= iheight / 2.0;
      } else if (ianchor.isBottom) {
        iy -= iheight;
      }

      shape.setFrame(ix,iy,iwidth,iheight);

      g.setStroke(lineStroke(item));
      g.setPaint(fillPaint(item));
      g.fill(shape);
      g.setPaint(linePaint(item));
      g.draw(shape);
    }
    g.setPaint(originalPaint);
    g.setStroke(originalStroke);
  }
}

case class Wedges[D](
  data : Iterable[D],
  x : Attribute[D,Double], y : Attribute[D,Double],
  startAngle : Attribute[D,Double], angle : Attribute[D,Double],
  innerRadius : Attribute[D,Double], outerRadius : Attribute[D,Double],
  linePaint : Attribute[D,Paint] = Plotter.defaultLinePaint,
  lineStroke  : Attribute[D,Stroke] = Plotter.defaultStroke,
  fillPaint : Attribute[D,Paint] = Plotter.defaultFillPaint
) extends Plotter {
  def render(g : Graphics2D) = {
    val arc1 = new Arc2D.Double();
    val arc2 = new Arc2D.Double()
    val shape = new Path2D.Double();
    
    def prepare(x : Double, y : Double, innerRadius : Double, outerRadius : Double, start : Double, angle : Double) = {
      shape.reset();
      val startDegrees = start*180/math.Pi;
      val angleDegrees = angle*180/math.Pi;
      arc1.setArcByCenter(x,y,innerRadius,startDegrees,angleDegrees,Arc2D.OPEN);
      arc2.setArcByCenter(x,y,outerRadius,startDegrees+angleDegrees,-angleDegrees,Arc2D.OPEN);
      shape.append(arc1,false);
      shape.append(arc2,true);
      shape.closePath();
    }
    
    val originalPaint = g.getPaint;
    val originalStroke = g.getStroke;
    for (item <- data) {
      prepare(x(item),y(item),innerRadius(item),outerRadius(item),startAngle(item),angle(item));
      g.setStroke(lineStroke(item));
      g.setPaint(fillPaint(item));
      g.fill(shape);
      g.setPaint(linePaint(item));
      g.draw(shape);
    }
    g.setPaint(originalPaint);
    g.setStroke(originalStroke);
  }
}

/**
 * Draws an area between two lines.
 *
 * @author dramage
 */
case class Area[D](data : Iterable[D],
  x : D=>Double,
  bottom : Attribute[D,Double],
  top : Attribute[D,Double],
  linePaint : Paint = PaintScale.transparent,
  lineStroke : Stroke = Plotter.defaultStroke,
  fillPaint : Paint = Plotter.defaultFillPaint
) extends Plotter {
  /** The shape of the area. */
  lazy val shape : GeneralPath = {
    val _s = new GeneralPath();
    
    _s.moveTo(x(data.head), top(data.head));
    for (item <- data.iterator.drop(1)) {
      _s.lineTo(x(item), top(item));
    }
    _s.lineTo(x(data.last), bottom(data.last));
    for (item <- data.toList.reverse.drop(1)) {
      _s.lineTo(x(item), bottom(item));
    }
    _s.lineTo(x(data.head), top(data.head));
    _s.closePath();
    
    _s;
  }

  def render(g : Graphics2D) = {
    val originalPaint = g.getPaint;
    val originalStroke = g.getStroke;

    g.setPaint(fillPaint);
    g.fill(shape);

    g.setPaint(linePaint);
    g.setStroke(lineStroke);
    g.draw(shape);

    g.setPaint(originalPaint);
    g.setStroke(originalStroke);
  }
}

/**
 * Draws a line connecting consecutive items in the given data using
 * the given linePaint and lineStroke.
 *
 * @author dramage
 */
case class Line[D](data : Iterable[D],
  x : D=>Double, y : D=>Double,
  linePaint : Paint = Plotter.defaultLinePaint,
  lineStroke : Stroke = Plotter.defaultStroke
) extends Plotter {
  def render(g : Graphics2D) = {
    val shape = new Line2D.Double();
    val originalPaint = g.getPaint;
    val originalStroke = g.getStroke;
    g.setPaint(linePaint);
    g.setStroke(lineStroke);
    for ((i1,i2) <- data.iterator zip data.iterator.drop(1)) {
      shape.setLine(x(i1),y(i1),x(i2),y(i2));
      g.draw(shape);
    }
    g.setPaint(originalPaint);
    g.setStroke(originalStroke);
  }
}

/**
 * Draws a bunch of lines.
 */
case class Lines[D](data : Iterable[Iterable[D]],
  x : D=>Double, y : D=>Double,
  linePaint : Paint = Plotter.defaultLinePaint,
  lineStroke : Stroke = Plotter.defaultStroke
) extends Plotter {
  def render(g : Graphics2D) = {
    for (line <- data) Line(line, x, y, linePaint, lineStroke).render(g);
  }
}

/**
 * Renders the text for each data items at the given x, y
 * positions.  The text is rendered relative to the given
 * anchor, i.e. so that the x,y position given is at the
 * BottomLeft of the rendered text, TopRight, etc.  For
 * a left or right anchor, adds hpadding pixels between the
 * text and the x coordinate.  For a top or bottom anchor,
 * adds vpadding pixels between the text and the y coordinate.
 * Both padding values default to 4.  The text is rendered at
 * the given angle.
 *
 * @author dramage
 */
case class Labels[D](data : Iterable[D],
  x : D=>Double, y : D=>Double,
  text : D=>String,
  anchor : Attribute[D,Anchor],
  angle : Attribute[D,Double] = 0.0,
  color : Attribute[D,Color] = Plotter.defaultLabelColor,
  font : Font = Plotter.defaultLabelFont,
  hoffset : Double = 0,
  voffset : Double = 0
) extends Plotter {
  def render(g : Graphics2D) = {
    val originalColor = g.getColor;
    
    val originalFont = g.getFont;
    g.setFont(font);
    val frc = g.getFontRenderContext();

    val originalTransform = g.getTransform();
    val transform = new AffineTransform();
    
    for (item <- data) {
      val itext = text(item);
      var ix = x(item);
      var iy = y(item);
      val ianchor = anchor(item);
      val iangle = angle(item);

      val layout = new TextLayout(itext, font, frc);
      val width = layout.getBounds.getWidth;
      val height = layout.getAscent;

      val pivotx =
        if (ianchor.isLeft) 0.0
        else if (ianchor.isCenter) - width / 2.0
        else /* if (ianchor.isRight) */ - width.toDouble;
      
      val pivoty =
        if (ianchor.isTop) layout.getAscent.toDouble;
        else if (ianchor.isMiddle) layout.getAscent / 2.0;
        else /* if (ianchor.isBottom) */ 0.0;
      
      transform.setToIdentity();
      transform.translate(ix+hoffset,iy+voffset);
      transform.rotate(iangle);
      transform.translate(pivotx,pivoty);
      g.transform(transform);

      g.setColor(color(item));
      layout.draw(g, 0f, 0f);
      
      g.setTransform(originalTransform);
    }
    
    g.setFont(originalFont);
    g.setColor(originalColor);
  }
}

sealed trait Anchor {
  val isLeft   : Boolean = getClass.getName.contains("Left");
  val isCenter : Boolean = getClass.getName.contains("Center");
  val isRight  : Boolean = getClass.getName.contains("Right");
  val isTop    : Boolean = getClass.getName.contains("Top");
  val isMiddle : Boolean = getClass.getName.contains("Middle");
  val isBottom : Boolean = getClass.getName.contains("Bottom");
}

object Anchor {
  object TopLeft extends Anchor
  object TopCenter extends Anchor
  object TopRight extends Anchor
  object MiddleLeft extends Anchor
  object MiddleCenter extends Anchor
  object MiddleRight extends Anchor
  object BottomLeft extends Anchor
  object BottomCenter extends Anchor
  object BottomRight extends Anchor
}

object ChartMain {
  def hist[K:Ordering](dist : Map[K,Int]) = {
    val xaxis = OrdinalScale(dist.keys.toList.sorted.toIndexedSeq).range(40,560)
    val yaxis = LinearScale(0, dist.values.max, 1).range(40, 560)
    
    val bars = Bars[(K,Int)](dist,
      x = ((tup : (K,Int)) => tup._1) andThen xaxis,
      y = (tup : (K,Int)) => yaxis.pxMax,
      width = xaxis.pxBand,
      height = ((tup : (K,Int)) => tup._2.toDouble) andThen yaxis.extent,
      anchor = Anchor.BottomCenter);
      
    Chart(600, 600).
      add(bars,
          xaxis.horizontal(yaxis.pxMax, voffset=4),
          yaxis.vertical(xaxis.pxMin)).
      show("Histogram");
  }

  def main(args : Array[String]) {
    val xaxis = LinearScale(min = 0, max = 10, tick = 1).range(40,560);
    val yaxis = LogScale(logMin = 0, logMax = 2, base = 10).range(560,40);
    
    val data = Array.tabulate(21)(i => (i / 2.0, i * i / 4.0));
    
    val dots : Dots[(Double,Double)] = Dots(data,
      x = ((t : (Double,Double)) => t._1) andThen xaxis,
      y = ((t : (Double,Double)) => t._2) andThen yaxis,
      r = 4.0,
      linePaint = PaintScale.blue,
      lineStroke = Plotter.defaultStroke,
      fillPaint = PaintScale.gray
    );
    
    val line = Line(data,
      x = dots.x,
      y = dots.y
    );
    
    val bars = Bars(data,
      x = dots.x,
      y = dots.y,
      width = 4.0,
      height = (t : (Double,Double)) => yaxis.extent(t._2),
      anchor = Anchor.TopCenter,
      linePaint = PaintScale.green.darker,
      lineStroke = Plotter.defaultStroke,
      fillPaint = PaintScale.green
    )
    
    val labels = Labels[((Double,Double),(Double,Double))](
      data = for (((d,r),i) <- (dots.data zip dots.anchors(Anchor.MiddleLeft)).zipWithIndex; if i > 0 && i % 5 == 0) yield (d,r),
      x = _._2._1,
      y = _._2._2,
      text = _._1.toString,
      anchor = Anchor.MiddleRight
    );
    
    val shape : Shape = new Ellipse2D.Double(-4,-6,8,12)
    val shapes = Shapes(data, x = dots.x, y = dots.y, shape = shape,
      anchor = Anchor.MiddleCenter, angle = 1.2);
    
    Chart(600, 600).
      add(dots, line, bars, labels, shapes,
          Wedges(Array(0.5,1.2,2.1),
            x = 100.0, y = 100.0,
            innerRadius = 20.0, outerRadius = 60.0,
            startAngle = (d : Double) => d,
            angle = math.Pi/8),
          Area(Array((1.,2.),(.5,3.),(-2.,1.5)).zipWithIndex,
            x = (t : ((Double,Double),Int))   => t._2 * 25 + 200.0,
            top    = (t : ((Double,Double),Int)) => t._1._1 * 25+200,
            bottom = (t : ((Double,Double),Int)) => t._1._2 * 25+200),
          xaxis.horizontal(yaxis(1)),
          yaxis.vertical(xaxis.pxMin)).
      show();
    
    hist(Map("Tofu"->2,"Cats"->7,"Linux"->4));
    
    Driving.chart.show()
  }
}


object Driving {
  val w = 900;
  val h = 590;
  val xs = LinearScale(3380,10500,2000).range(50,w);
  val ys = LinearScale(1.25,3.49,.5).range(h,50);

  case class Record(anchor : Anchor, year : Int, miles : Int, gas : Double);

  val driving = List(
  Record(Anchor.MiddleRight, 1956, 3675, 2.38),
  Record(Anchor.MiddleLeft, 1957, 3706, 2.40),
  Record(Anchor.TopCenter, 1958, 3766, 2.26),
  Record(Anchor.BottomCenter, 1959, 3905, 2.31),
  Record(Anchor.MiddleLeft, 1960, 3935, 2.27),
  Record(Anchor.TopCenter, 1961, 3977, 2.25),
  Record(Anchor.MiddleLeft, 1962, 4085, 2.22),
  Record(Anchor.TopCenter, 1963, 4218, 2.12),
  Record(Anchor.TopCenter, 1964, 4369, 2.11),
  Record(Anchor.TopCenter, 1965, 4538, 2.14),
  Record(Anchor.BottomCenter, 1966, 4676, 2.14),
  Record(Anchor.TopCenter, 1967, 4827, 2.14),
  Record(Anchor.MiddleLeft, 1968, 5038, 2.13),
  Record(Anchor.MiddleLeft, 1969, 5207, 2.07),
  Record(Anchor.MiddleLeft, 1970, 5376, 2.01),
  Record(Anchor.TopCenter, 1971, 5617, 1.93),
  Record(Anchor.TopCenter, 1972, 5973, 1.87),
  Record(Anchor.MiddleLeft, 1973, 6154, 1.90),
  Record(Anchor.MiddleRight, 1974, 5943, 2.34),
  Record(Anchor.TopCenter, 1975, 6111, 2.31),
  Record(Anchor.TopCenter, 1976, 6389, 2.32),
  Record(Anchor.MiddleLeft, 1977, 6630, 2.36),
  Record(Anchor.TopCenter, 1978, 6883, 2.23),
  Record(Anchor.MiddleRight, 1979, 6744, 2.68),
  Record(Anchor.MiddleRight, 1980, 6672, 3.30),
  Record(Anchor.MiddleLeft, 1981, 6732, 3.30),
  Record(Anchor.MiddleLeft, 1982, 6835, 2.92),
  Record(Anchor.MiddleLeft, 1983, 6943, 2.66),
  Record(Anchor.MiddleLeft, 1984, 7130, 2.48),
  Record(Anchor.MiddleLeft, 1985, 7323, 2.36),
  Record(Anchor.MiddleRight, 1986, 7558, 1.76),
  Record(Anchor.BottomCenter, 1987, 7770, 1.76),
  Record(Anchor.TopCenter, 1988, 8089, 1.68),
  Record(Anchor.MiddleRight, 1989, 8397, 1.75),
  Record(Anchor.BottomCenter, 1990, 8529, 1.88),
  Record(Anchor.MiddleLeft, 1991, 8535, 1.78),
  Record(Anchor.MiddleLeft, 1992, 8662, 1.69),
  Record(Anchor.MiddleRight, 1993, 8855, 1.60),
  Record(Anchor.TopCenter, 1994, 8909, 1.59),
  Record(Anchor.TopCenter, 1995, 9150, 1.60),
  Record(Anchor.BottomCenter, 1996, 9192, 1.67),
  Record(Anchor.MiddleLeft, 1997, 9416, 1.65),
  Record(Anchor.TopCenter, 1998, 9590, 1.39),
  Record(Anchor.MiddleLeft, 1999, 9687, 1.50),
  Record(Anchor.BottomCenter, 2000, 9717, 1.89),
  Record(Anchor.MiddleRight, 2001, 9699, 1.77),
  Record(Anchor.TopCenter, 2002, 9814, 1.64),
  Record(Anchor.MiddleLeft, 2003, 9868, 1.86),
  Record(Anchor.MiddleRight, 2004, 9994, 2.14),
  Record(Anchor.MiddleRight, 2005, 10067, 2.53),
  Record(Anchor.MiddleLeft, 2006, 10037, 2.79),
  Record(Anchor.MiddleLeft, 2007, 10025, 2.95),
  Record(Anchor.MiddleRight, 2008, 9880, 3.31),
  Record(Anchor.TopCenter, 2009, 9657, 2.38),
  Record(Anchor.MiddleRight, 2010, 9596, 2.61));

  val chart = Chart(w+50,h+50);

  val x = (r : Record) => xs(r.miles);
  
  val y = (r : Record) => ys(r.gas);

  val line = Line(driving, x, y,
    linePaint = PaintScale.black
  );
  
  val dots = Dots(driving, x, y, r = 4.0,
    fillPaint = PaintScale.white);
  
  val labels = Labels(driving, x, y,
    text = (r : Record) => r.year.toString,
    anchor = (r : Record) => r.anchor);
  
  chart.add(
    xs.horizontal(top = ys.pxMax), ys.vertical(left = xs.pxMin),
    line,dots,labels)
}

