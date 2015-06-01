package test

import quadtree._

import scala.Unit

/**
 * Created by merlin on 5/30/15.
 */
class testScala {

   def rangefunction(pt:Point, rect:Rectangle):Point=
   {
      printf(pt.x+" , "+pt.y+" rectagnel w: "+" " + rect.h)
      pt
   }

   def navigation(searchfunction:(Point, Rectangle)=>Point, label:Boolean, pt:Point, Rect:Rectangle)=
   {
      if(label)
      {
        searchfunction(pt,Rect)
      }else
      {
        printf("xxxxxxx")
      }

   }



   object testFunction extends App{

     val point1=new Point(2, 7)
     val rectangle=new Rectangle(0,0,100,100)

      navigation(rangefunction, true,point1,rectangle)

   }

}
