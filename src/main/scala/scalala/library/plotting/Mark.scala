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
import java.awt.geom.{AffineTransform,Point2D,Line2D,Ellipse2D,Rectangle2D,Arc2D,Path2D};
import javax.swing.{JFrame,JScrollPane,WindowConstants};
import java.io.File;

trait Mark[D] extends Plotter {
  /** The parent of this mark or null if it is a root. */
  def parent : Mark[D] =
    null;

  // val children : scala.collection.mutable.Buffer[Mark[_]];

  protected def missing(missing : String) =
    throw new IllegalArgumentException("Required property " + missing + " not defined");

  /** The data of this mark. */
  def data : Iterable[D] =
    if (parent != null) parent.data else missing("data");
  
  /** An attribute describing the x location of each item. */
  private var _x : Option[Attribute[D,Double]] =
    if (parent == null) None else parent._x;
  def x : Attribute[D,Double] =
    if (_x.isDefined) _x.get else missing("x");
  def x_=(attr : Attribute[D,Double]) =
    _x = Some(attr);
  
  /** An attribute describing the y location of each item. */
  private var _y : Option[Attribute[D,Double]] =
    if (parent == null) None else parent._y;
  def y : Attribute[D,Double] =
    if (_y.isDefined) _y.get else missing("y");
  def y_=(attr : Attribute[D,Double]) =
    _y = Some(attr);
  
  /** An attribute describing the width of each item. */
  def width : Attribute[D,Double] =
    if (parent != null) parent.width else missing("width");
  
  /** An attribute describing the height of each item. */
  def height : Attribute[D,Double] =
    if (parent != null) parent.height else missing("height");

  /** An attribute describing the x offset of the given item relative to a starting drawing position of 0, 0 */
  def offsetx : Attribute[D,Double] =
    if (parent != null) parent.offsetx else 0.0;

  /** An attribute describing the y offset of the given item relative to a starting drawing position of 0, 0 */
  def offsety : Attribute[D,Double] =
    if (parent != null) parent.offsety else 0.0;

  /** The anchor location of the given item. */
  def anchor : Attribute[D,Anchor] =
    if (parent != null) parent.anchor else Anchor.MiddleCenter;
  
  /** The angle to render the item relative to its anchor. */
  def angle : Attribute[D,Double] =
    if (parent != null) parent.angle else 0.0;
  
  /** An attribute describing whether each item is visible. */  
  def visible : Attribute[D,Boolean] =
    if (parent != null) parent.visible else true;
  
  /**
   * Applies the appropriate transform to render this item into
   * the given transform, which defaults to a new identity transform.
   *
   * @return transform, after applying our transformation
   */
  def getItemTransform(item : D, transform : AffineTransform = new AffineTransform)
  : AffineTransform = {
    val iwidth = width(item);
    val iheight = height(item);
    val ianchor = anchor(item);
    transform.translate(x(item),y(item));
    transform.rotate(angle(item));
    transform.translate(-offsetx(item), -offsety(item));
    transform.translate(
      if (ianchor.isRight)  -iwidth  else if (ianchor.isCenter) -iwidth/2 else 0,
      if (ianchor.isBottom) -iheight else if (ianchor.isMiddle) -iheight/2 else 0);
    return transform;
  }
  
  /**
   * Computes the bounding area of this item given its angle,
   * anchor, x, y, width, and height properties.  Sets the
   * computed bounds into the given target, defaulting to a
   * new Rectangle2D.double.
   *
   * @return target, after setting its bounds.
   */
  def getItemBounds(item : D, target : Rectangle2D.Double = new Rectangle2D.Double)
  : Rectangle2D.Double = {
    val ianchor = anchor(item);
    val iangle = angle(item);
    val iwidth  = width(item)  * math.cos(iangle);
    val iheight = height(item) * math.sin(iangle);
    
    var ix = x(item);
    if (ianchor.isRight)       ix -= iwidth;
    else if (ianchor.isCenter) ix -= iwidth / 2;
    
    var iy = y(item);
    if (ianchor.isBottom)      iy -= iheight;
    else if (ianchor.isMiddle) iy -= iheight / 2;
    
    target.setRect(ix,iy,iwidth,iheight);
    return target;
  }
  
  def getDataBounds(target : Rectangle2D.Double = new Rectangle2D.Double)
  : Rectangle2D.Double = {
    val itemRect = new Rectangle2D.Double();
    for (item <- data) target.add(getItemBounds(item,itemRect));
    return target;
  }
  
  override def render(g : Graphics2D) = {
  }
  
  class Dots extends XDots[D](this);
}


/**
 * Draws a shape for every data item in the given set of data.
 */
case class Shapes[D](
  override val data : Iterable[D],
  override val x : Attribute[D,Double],
  override val y : Attribute[D,Double],
  shape : Attribute[D,Shape],
  override val angle : Attribute[D,Double] = 0.0,
  override val anchor : Attribute[D,Anchor] = Anchor.MiddleCenter,
  linePaint : Attribute[D,Paint] = Plotter.defaultLinePaint,
  lineStroke : Attribute[D,Stroke] = Plotter.defaultStroke,
  fillPaint : Attribute[D,Paint] = Plotter.defaultFillPaint
) extends Mark[D] with Plotter {

  override val offsetx : Attribute[D,Double] =
    (item : D) => shape(item).getBounds.getX;
  
  override val offsety : Attribute[D,Double] =
    (item : D) => shape(item).getBounds.getY;

  override val width : Attribute[D,Double] =
    (item : D) => shape(item).getBounds.getWidth;
    
  override val height : Attribute[D,Double] =
    (item : D) => shape(item).getBounds.getHeight;

  override def render(g : Graphics2D) = {
    val originalPaint = g.getPaint;
    val originalStroke = g.getStroke;

    val originalTransform = g.getTransform;
    val renderTransform = new AffineTransform;
    
    for (item <- data) {
      val ishape = shape(item);
      
      renderTransform.setToIdentity;
      g.transform(getItemTransform(item,renderTransform))
      g.setPaint(fillPaint(item));
      g.fill(ishape);
      g.setPaint(linePaint(item));
      g.setStroke(lineStroke(item));
      g.draw(ishape);
      g.setTransform(originalTransform);
    }
    
    g.setPaint(originalPaint);
    g.setStroke(originalStroke);
  }
}

class XChart extends Mark[Unit] {
  class Data[D](override val data : Iterable[D]) extends Mark[D];
}

class XDots[D](override val parent : Mark[D]) extends Mark[D] {
  
  /** An attribute describing the y location of each item. */
  private var _r : Option[Attribute[D,Double]] = None;
  def r : Attribute[D,Double] =
    if (_r.isDefined) _r.get else missing("r");
  def r_=(attr : Attribute[D,Double]) =
    _r = Some(attr);

  override def render(g : Graphics2D) {
    super.render(g);
    
    val shape = new Ellipse2D.Double();
    
    val originalPaint = g.getPaint;
    val originalStroke = g.getStroke;
    for (item <- data) {
      val ir = r(item);
      val ix = x(item) - ir;
      val iy = y(item) - ir;
      val id = 2 * ir;

      shape.setFrame(ix,iy,id,id);
      
      //g.setStroke(lineStroke(item));
      //g.setPaint(fillPaint(item));
      g.fill(shape);
      //g.setPaint(linePaint(item));
      g.draw(shape);
    }
    g.setPaint(originalPaint);
    g.setStroke(originalStroke);
  }
}

object MarkMain {
  def main(args : Array[String]) {
    val c = new XChart {
      new Data(Array(1,2,3)) {
        new Dots {
          x = (i : Int) => i.toDouble
          y = (i : Int) => i*i.toDouble
        }
      }
    }
  }
}

