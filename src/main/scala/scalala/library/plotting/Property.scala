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

/**
 * A property is a value that may change.  When it does, it notifies
 * its listeners.
 *
 * @author dramage
 */
trait Property[@specialized V] {
  import java.lang.ref.WeakReference;
  import scala.collection.mutable.{ArrayBuffer};

  def value : V;
  
  protected val subscribers = new ArrayBuffer[WeakReference[V=>Unit]];

  /** Notifies all subscribers of the change in value. */
  def fire() = {
    val _value = value;
    var i = 0;
    while (i < subscribers.length) {
      val sub = subscribers(i).get;
      if (sub == null) {
        subscribers.remove(i);
      } else {
        sub(_value);
        i += 1;
      }
    }
  }
   
  /** Registers a new subscriber, stored only as a WeakReference. */
  def += (fn : V=>Unit) =
    subscribers += new WeakReference(fn);
  
  /** De-registers a subscriber. */
  def -= (fn : V=>Unit) = {
    var i = 0;
    while (i < subscribers.length) {
      if (subscribers(i).get == fn || subscribers(i).get == null) {
        subscribers.remove(i);
      } else {
        i += 1;
      }
    }
  }
  
  /** Returns a new property derived from this property. */
  def apply[@specialized R](fn : V=>R) =
    Property.from(this)(fn);
  
  /** Returns a builder for a derived property. */
  def &[U](other : Property[U]) =
    Property.Builder2[V,U](this,other);
    
  override def toString =
    "%s[%d listeners]=%s".format(this.getClass.getSimpleName, subscribers.length, value.toString)
}

object Property {
  /** Implicit conversion from any constant to a corresponding property. */
  implicit def constant[@specialized V](v : V) =
    new Constant(v);

  implicit def variable[@specialized V](v : V) = {
    val rv = new Variable[V];
    rv.value = v;
    rv;
  }

  /** Creates a new Property[V] derived from the given input property. */
  def from[@specialized P1,@specialized V](p1 : Property[P1])(fn : P1=>V) =
    new Derived1[P1,V](fn,p1);
  
  /** Creates a new Property[V] derived from the given input property. */
  def from[@specialized P1,@specialized P2,@specialized V](p1 : Property[P1], p2 : Property[P2])(fn : (P1,P2)=>V) =
    new Derived2[P1,P2,V](fn,p1,p2);
  
  //
  // Instantiations
  //
  
  /**
   * A Constant is a property whose value does not change.
   *
   * @author dramage
   */
  class Constant[@specialized V](override val value : V) extends Property[V];

  /**
   * A Variable is a Property that can be manually changed.
   *
   * @author dramage
   */
  class Variable[@specialized V] extends Property[V] {
    protected var _value : V = _;
    
    override def value = _value;
    
    def value_=(newValue : V) = {
      this._value = newValue;
      fire();
    }
  }

  /** A property derived from one other property. */
  class Derived1[@specialized P1,@specialized V]
  (fn : P1=>V, p1 : Property[P1])
  extends Property[V] {
    protected var valid = false;
    protected var cached : V = _;

    protected val update1 : (P1=>Unit) =
      (v1 : P1) => compute(v1);

    // register this property with the enclosing property
    p1 += update1;

    override def value = {
      if (!valid) {
        compute(p1.value);
      }
      cached;
    }
    
    def compute(v1 : P1) = {
      val oldCached = cached;
      val oldValid = valid;
      cached = fn(v1);
      valid = true;
      if (oldValid && cached != oldCached) fire();
    }
  }

  /** A property derived from two other properties. */
  class Derived2[@specialized P1,@specialized P2,@specialized V]
  (fn : (P1,P2)=>V, p1 : Property[P1], p2 : Property[P2])
  extends Property[V] {
    protected var valid = false;
    protected var cached : V = _;

    protected val update1 : (P1=>Unit) =
      (v1 : P1) => compute(v1, p2.value);
      
    protected val update2 : (P2=>Unit) =
      (v2 : P2) => compute(p1.value, v2);

    // register this property with the enclosing property
    p1 += update1;
    p2 += update2;

    override def value = {
      if (!valid) {
        compute(p1.value, p2.value);
      }
      cached;
    }
    
    def compute(v1 : P1, v2 : P2) = {
      val oldCached = cached;
      val oldValid = valid;
      cached = fn(v1, v2);
      valid = true;
      if (oldValid && cached != oldCached) fire();
    }
  }

  //
  // Builders for providing the property building dsl
  //

  /** Helper class for building Derived2. */
  case class Builder2[V1,V2](p1 : Property[V1], p2 : Property[V2]) {
    // def &[V3](other : Property[V3]) = new PropertyBuilder3[V1,V2,V3](p1,p2,p3);
    def apply[@specialized R](fn : (V1,V2)=>R) =
      Property.from(p1,p2)(fn);
  }
}

