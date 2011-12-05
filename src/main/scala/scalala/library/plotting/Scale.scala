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

/**
 * Converts items of type I to value between 0 and 1.
 *
 * @author dramage
 */
trait Scale[@specialized I] {
  def apply(item : I) : Double;
}

/**
 * A numeric scale mapping numbers from min to max to a real value between 0 and 1.
 *
 * @author dramage
 */
trait NumericScale extends Scale[Double] {
  /** The minimum value. */
  def min : Double;
  
  /** The maximum value. */
  def max : Double;
  
  /** Maps from a value to a normalized number between 0 and 1. */
  def apply(value : Double) : Double;
  
  /** Maps from a normalized value between 0 and 1 to a number in the scale. */
  def unapply(nvalue : Double) : Double;
  
  /** Returns tick positions for this scale. */
  def ticks : Array[Double];
  
  /** Returns label and positions for major ticks in this scale. */
  def labels : Array[(String,Double)];
  
  /** Creates a NumericPixelRange from this NumericScale to the given range of pixels. */
  def range(pxMin : Double, pxMax : Double) =
    NumericPixelRange(this, pxMin, pxMax);
  
  /** Formats a tick mark as a string. */
  def formatText(value : Double) : String;
}

object NumericScale {
  def scaleOf(x : Double) = (math.signum(x.abs-1) * (math.log(x.abs)/math.log(10)).abs.ceil);
  
  def floor(x : Double, decimal : Int) =
    (x * math.pow(10,decimal)).floor * math.pow(10,-decimal);
    
  def ceil(x : Double, decimal : Int) =
    (x * math.pow(10,decimal)).ceil * math.pow(10,-decimal);
    
  val maxAbsoluteError = java.lang.Double.MIN_VALUE * 1e3;
  val maxRelativeError = 1e-10;
  
  /** Based on http://www.cygnus-software.com/papers/comparingfloats/comparingfloats.htm */
  def approxEq(a : Double, b : Double) : Boolean = {
    if (a == b || math.abs(a - b) < maxAbsoluteError) return true;
    
    val relativeError =
      if (math.abs(b) > math.abs(a)) math.abs((a - b) / b) else math.abs((a - b) / a);
    
    return relativeError <= maxRelativeError;
  }
  
  def fudge(min : Double, max : Double, segments : Int = 10) = new (Double=>String) {
    val decimal : Int = math.max(-scaleOf((max - min).abs / segments).toInt,0);
    val step = ceil((max - min).abs / segments, decimal);
    val betterMin = if (min < max) floor(min,decimal) else max + step * segments;
    val betterMax = if (min < max) min + step * segments else floor(max,decimal);
    val format = {
      // if fudged min and max are close enough
      if (approxEq(min, betterMin) && approxEq(max, betterMax))
        "%."+decimal+"f"
      else
        "%."+(decimal+1)+"f";
    };
    def apply(v : Double) = format.format(v);
  }
}

/**
 * A linear scale from min to max with major
 *
 * @author dramage
 */
case class LinearScale(val min : Double, val max : Double, val tick : Double)
extends NumericScale {
  require(min < max, "Min must be less than max");
  require(tick <= max - min, "Tick must be less than or equal to extent");

  override def apply(value : Double) =
    (value - min) / (max - min)

  override def unapply(vv : Double) =
    vv * (max - min) + min
  
  def ticks = {
    def floorUnlessClose(dbl : Double) =
      if (math.abs(dbl - dbl.round) < tick * 1e-6) dbl.round else dbl.floor
      
    val nTicks = floorUnlessClose((max - min) / tick).toInt + 1;
    val minTick = math.ceil(min / tick) * tick;
    Array.tabulate(nTicks)(i => minTick + i * tick).filter(x => x >= min && x <= max);
  }
  
  def labels = {
    for (tick <- ticks) yield formatText(tick) -> tick
  }

  def rounded =
    LinearScale(min - (min % tick), max + (max % tick), tick)
  
  /** Formats a tick mark as a string. */
  def formatText(value : Double) : String =
    ("%."+math.max(-NumericScale.scaleOf(tick),0).toInt+"f").format(value);
}

/**
 * A logarithmic scale from logMin to logMax in the given base.
 *
 * @author dramage
 */
case class LogScale(val logMin : Double, val logMax : Double, val base : Int)
extends NumericScale {
  require(min < max, "Min must be less than max");
  require(min > 0 && max > 0, "LogScale domain must be positive");
  
  override def min = math.pow(base, logMin);
  override def max = math.pow(base, logMax);
  
  override def apply(value : Double) =
    (math.log(value) / math.log(base) - logMin) / (logMax - logMin);
  
  override def unapply(vv : Double) =
    throw new UnsupportedOperationException;
  
  def ticks = {
    (for (exponent <- math.floor(logMin).toInt to math.ceil(logMax).toInt;
          tick <- 0 until base;
          val value = math.log(math.pow(base,exponent) * tick) / math.log(base);
          if value >= logMin && value <= logMax) yield math.pow(base,value)
    ).toArray;
  }
  
  def labels = {
    (for (exponent <- math.floor(logMin).toInt to math.ceil(logMax).toInt;
          if exponent >= logMin && exponent <= logMax)
     yield (base+"^"+exponent) -> math.pow(base,exponent)
    ).toArray;
  }
  
  def rounded =
    LogScale(math.floor(logMin), math.ceil(logMax), base)
  
  /** Formats a tick mark as a string. */
  def formatText(value : Double) : String =
    base+"^"+(math.log(value)/math.log(base))
}

object LogScale {
  def fromData(data : Traversable[Double], base : Int = 10) = {
    val logMin = math.log(data.min) / math.log(base);
    val logMax = math.log(data.max) / math.log(base);
    LogScale(logMin, logMax, base);
  }
}

/**
 * Constructs a scale for the given ordinal items, evenly dividing
 * the range into items.size bins.  The width allocated to each item within
 * its bin is controlled by the band parameter.
 *
 * @param band Determines the width of each band in the scale.  Normally,
 *   this number is a percentage between 0 and 1 reflecting the fraction
 *   of the segment allocated to the band.  If negative, band is interpreted
 *   as a literal number of pixels as a margin between successive elements.
 *
 * @author dramage
 */
case class OrdinalScale[I](items : IndexedSeq[I], band : Double = 0.8)
extends Scale[I] {
  
  private val itemToIndex = items.zipWithIndex.toMap;

  def range(pxMin : Double, pxMax : Double) =
    OrdinalPixelRange(this, pxMin, pxMax);
  
  def apply(item : I) =
    (itemToIndex(item) + 0.5) / items.size
}


/**
 * Assigns values of type I to a pixel range.
 *
 * @author dramage
 */
trait PixelRange[@specialized I] extends (I=>Double) {
  /** The underlying scale on which this pixel range is based. */
  def scale : Scale[I];

  /** Map from the input range to the output pixel range. */
  def apply(v : I) : Double =
    scale(v) * pxLength + pxMin;

  ///** Maps from a pixel value back to the underlying numeric scale. */
  //def unapply(pxValue : Double) : Double =
  //  scale.unapply((pxValue - pxMin) / pxLength);

  /** What is the extent in pixels of the given value (from logical 0). */
  def extent(v : I) : Double =
    scale(v) * pxLength;

  /** The minimum value in the output scale. */
  def pxMin : Double;
  
  /** The maximum value in the output scale. */
  def pxMax : Double;
  
  /** The length of the output scale. */
  final def pxLength : Double =
    pxMax - pxMin;
}

/**
 * A mapping from a NumericScale to a pixel position.
 *
 * @author dramage
 */
case class NumericPixelRange(
  override val scale : NumericScale,
  override val pxMin : Double,
  override val pxMax : Double)
extends PixelRange[Double] { self =>
  def horizontal(top : Double,
    labelText : (Double=>String) = null,
    labelFont : Font = Plotter.defaultLabelFont,
    labelColor : Attribute[String,Color] = Plotter.defaultLabelColor,
    linePaint : Paint = Plotter.defaultLinePaint,
    lineStroke : Stroke = Plotter.defaultStroke
  ) = new Plotter {
    val baseline =
      HRule(left=pxMin, width=pxLength, top=top, linePaint=linePaint, lineStroke=lineStroke);
      
    val ticklines =
      scale.ticks.map(tick =>
        VRule(left=self.apply(tick), top=top-4, height=8,
          linePaint=linePaint, lineStroke=lineStroke));
      
    val labels =
      Labels(data = scale.labels,
        x = (tup : (String,Double)) => apply(tup._2),
        y = (tup : (String,Double)) => top,
        text = (tup : (String,Double)) => tup._1,
        voffset = 6.0,
        anchor = Anchor.TopCenter,
        font = labelFont,
        color = (tup : (String,Double)) => labelColor(tup._1)
      );
  
    def render(g : Graphics2D) {
      baseline.render(g);
      ticklines.foreach(_.render(g));
      labels.render(g);
    }
  }
  
  def vertical(left : Double,
    labelText : (Double=>String) = null,
    labelFont : Font = Plotter.defaultLabelFont,
    labelColor : Attribute[String,Color] = Plotter.defaultLabelColor,
    linePaint : Paint = Plotter.defaultLinePaint,
    lineStroke : Stroke = Plotter.defaultStroke
  ) = new Plotter {
    val baseline =
      VRule(left=left, top=pxMin, height=pxLength, linePaint=linePaint, lineStroke=lineStroke);
    
    val ticklines =
      scale.ticks.map(tick =>
        HRule(left=left-4, top=self.apply(tick), width=8,
          linePaint=linePaint, lineStroke=lineStroke));
      
    val labels =
      Labels(data = scale.labels,
        x = (tup : (String,Double)) => left,
        y = (tup : (String,Double)) => apply(tup._2),
        text = (tup : (String,Double)) => tup._1,
        hoffset = -8.0,
        anchor = Anchor.MiddleRight,
        font = labelFont,
        color = (tup : (String,Double)) => labelColor(tup._1)
      );
  
    def render(g : Graphics2D) {
      baseline.render(g);
      ticklines.foreach(_.render(g));
      labels.render(g);
    }
  }
}


/**
 * A mapping from an OrdinalScale to a pixel position.
 *
 * @author dramage
 */
case class OrdinalPixelRange[I](
  override val scale : OrdinalScale[I],
  override val pxMin : Double,
  override val pxMax : Double)
extends PixelRange[I] { 
  
  /** The width of each band in pixels. */
  val pxBand = {
    if (scale.band < 0)
      (pxLength / scale.items.size) + scale.band
    else
      (pxLength / scale.items.size) * scale.band
  }
  
  def horizontal(top : Double,
    angle : Attribute[I,Double] = 0.0, anchor : Attribute[I,Anchor] = Anchor.TopCenter,
    labelText : (I=>String) = (item : I) => item.toString,
    labelFont : Font = Plotter.defaultLabelFont,
    labelColor : Attribute[I,Color] = Plotter.defaultLabelColor,
    hoffset : Double = 0.0,
    voffset : Double = 0.0,
    linePaint : Paint = Plotter.defaultLinePaint,
    lineStroke : Stroke = Plotter.defaultStroke
  ) = new {
    val baseline =
      HRule(left = pxMin, width = pxLength, top = top, linePaint = linePaint, lineStroke = lineStroke);
      
    val labels =
      Labels(data = scale.items,
        x = (v : I) => apply(v),
        y = (v : I) => top,
        text = labelText,
        angle = angle,
        anchor = anchor,
        hoffset = hoffset,
        voffset = voffset,
        font = labelFont,
        color = labelColor
      );
  } with Plotter {
    def render(g : Graphics2D) {
      baseline.render(g);
      labels.render(g);
    }
  }
  
  def vertical(left : Double,
    angle : Attribute[I,Double] = 0.0, anchor : Attribute[I,Anchor] = Anchor.MiddleRight,
    labelText : (I=>String) = (item : I) => item.toString,
    labelFont : Font = Plotter.defaultLabelFont,
    labelColor : Attribute[I,Color] = Plotter.defaultLabelColor,
    hoffset : Double = 0.0,
    voffset : Double = 0.0,
    linePaint : Paint = Plotter.defaultLinePaint,
    lineStroke : Stroke = Plotter.defaultStroke
  ) = new {
    val baseline =
      VRule(left = left, top = pxMin, height = pxLength, linePaint = linePaint, lineStroke = lineStroke);
      
    val labels =
      Labels(data = scale.items,
        x = (v : I) => left,
        y = (v : I) => apply(v),
        text = labelText,
        angle = angle,
        anchor = anchor,
        hoffset = hoffset,
        voffset = voffset,
        font = labelFont,
        color = labelColor
      );
  } with Plotter {
    def render(g : Graphics2D) {
      baseline.render(g);
      labels.render(g);
    }
  }
}

