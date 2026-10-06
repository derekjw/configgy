/*
 * Copyright 2009 Robey Pointer <robeypointer@gmail.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License. You may obtain
 * a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.lag.configgy

import org.specs2.mutable.Specification
import scala.util.Sorting


class AttributesSpec extends Specification {

  "Attributes" should {
    "set values" in {
      val s = new Attributes(null, "root")
      s.toString mustEqual "{root: }"
      s.setString("name", "Communist")
      s.toString mustEqual "{root: name=\"Communist\" }"
      s.setInt("age", 8)
      s.toString mustEqual "{root: age=\"8\" name=\"Communist\" }"
      s.setInt("age", 19)
      s.toString mustEqual "{root: age=\"19\" name=\"Communist\" }"
      s.setBool("sleepy", true)
      s.toString mustEqual "{root: age=\"19\" name=\"Communist\" sleepy=\"true\" }"

      // try both APIs.
      val s2 = new Attributes(null, "root")
      s2.toString mustEqual "{root: }"
      s2("name") = "Communist"
      s2.toString mustEqual "{root: name=\"Communist\" }"
      s2("age") = 8
      s2.toString mustEqual "{root: age=\"8\" name=\"Communist\" }"
      s2("age") = 19
      s2.toString mustEqual "{root: age=\"19\" name=\"Communist\" }"
      s2("sleepy") = true
      s2.toString mustEqual "{root: age=\"19\" name=\"Communist\" sleepy=\"true\" }"
    }

    "get values" in {
      val s = new Attributes(null, "root")
      s("name") = "Communist"
      s("age") = 8
      s("sleepy") = true
      s("money") = 1900500400300L
      s.getString("name", "") mustEqual "Communist"
      s.getInt("age", 999) mustEqual 8
      s.getInt("unknown", 500) mustEqual 500
      s.getLong("money", 0) mustEqual 1900500400300L
      s("name") mustEqual "Communist"
      s("age", null) mustEqual "8"
      s("age", "500") mustEqual "8"
      s("age", 500) mustEqual 8
      s("unknown", "500") mustEqual "500"
      s("money", 0L) mustEqual 1900500400300L
      s("money").toLong mustEqual 1900500400300L
      s("age").toInt mustEqual 8
      s("sleepy").toBoolean mustEqual true
    }

    "case-preserve keys in get/set" in {
      val s = new Attributes(null, "")
      s("Name") = "Communist"
      s("AGE") = 8
      s("Name") mustEqual "Communist"
      s("naME", "") mustEqual ""
      s("age", 0) mustEqual 0
      s("AGE") mustEqual "8"
    }

    "set compound values" in {
      val s = new Attributes(null, "")
      s("name") = "Communist"
      s("age") = 8
      s("disposition") = "fighter"
      s("diet.food") = "Meow Mix"
      s("diet.liquid") = "water"
      s("data") = "\r\r\u00ff\u00ff"
      s.toString mustEqual ("{: age=\"8\" data=\"\\r\\r\\xff\\xff\" diet={diet: food=\"Meow Mix\" liquid=\"water\" } " +
        "disposition=\"fighter\" name=\"Communist\" }")
    }

    "know what it contains" in {
      val s = new Attributes(null, "")
      s("name") = "Communist"
      s("age") = 8
      s("diet.food") = "Meow Mix"
      s("diet.liquid") = "water"
      s.toString mustEqual "{: age=\"8\" diet={diet: food=\"Meow Mix\" liquid=\"water\" } name=\"Communist\" }"
      s.contains("age") must beTrue
      s.contains("unknown") must beFalse
      s.contains("diet.food") must beTrue
      s.contains("diet.gas") must beFalse
      s.toString mustEqual "{: age=\"8\" diet={diet: food=\"Meow Mix\" liquid=\"water\" } name=\"Communist\" }"
    }

    "auto-vivify" in {
      val s = new Attributes(null, "")
      s("a.b.c") = 8
      s.toString mustEqual "{: a={a: b={a.b: c=\"8\" } } }"
      s.getString("a.d.x") must beNone
      // shouldn't have changed the attr map:
      s.toString mustEqual "{: a={a: b={a.b: c=\"8\" } } }"
    }

    "compare with ==" in {
      val s = new Attributes(null, "root")
      s("name") = "Communist"
      s("age") = 8
      s("diet.food.dry") = "Meow Mix"
      val t = new Attributes(null, "root")
      t("name") = "Communist"
      t("age") = 8
      t("diet.food.dry") = "Meow Mix"
      s mustEqual t
    }

    "remove values" in {
      val s = new Attributes(null, "")
      s("name") = "Communist"
      s("age") = 8
      s("diet.food") = "Meow Mix"
      s("diet.liquid") = "water"
      s.toString mustEqual "{: age=\"8\" diet={diet: food=\"Meow Mix\" liquid=\"water\" } name=\"Communist\" }"
      s.remove("diet.food") must beTrue
      s.remove("diet.food") must beFalse
      s.toString mustEqual "{: age=\"8\" diet={diet: liquid=\"water\" } name=\"Communist\" }"
    }

    "convert to a map" in {
      val s = new Attributes(null, "")
      s("name") = "Communist"
      s("age") = 8
      s("disposition") = "fighter"
      s("diet.food") = "Meow Mix"
      s("diet.liquid") = "water"
      val map = s.asMap

      // turn it into a sorted list, so we get a deterministic answer
      val keyList = map.keys.toList.toArray
      Sorting.quickSort(keyList)
      (for (k <- keyList) yield (k + "=" + map(k))).mkString("{ ", ", ", " }") mustEqual
        "{ age=8, diet.food=Meow Mix, diet.liquid=water, disposition=fighter, name=Communist }"
    }

    "copy" in {
      val s = new Attributes(null, "")
      s("name") = "Communist"
      s("age") = 8
      s("diet.food") = "Meow Mix"
      s("diet.liquid") = "water"
      val t = s.copy()

      s.toString mustEqual "{: age=\"8\" diet={diet: food=\"Meow Mix\" liquid=\"water\" } name=\"Communist\" }"
      t.toString mustEqual "{: age=\"8\" diet={diet: food=\"Meow Mix\" liquid=\"water\" } name=\"Communist\" }"

      s("diet.food") = "fish"

      s.toString mustEqual "{: age=\"8\" diet={diet: food=\"fish\" liquid=\"water\" } name=\"Communist\" }"
      t.toString mustEqual "{: age=\"8\" diet={diet: food=\"Meow Mix\" liquid=\"water\" } name=\"Communist\" }"
    }

    "copy with inheritance" in {
      val s = new Attributes(null, "s")
      s("name") = "Communist"
      s("age") = 1
      val t = new Attributes(null, "t")
      t("age") = 8
      t("disposition") = "hungry"
      t.inheritFrom = Some(s)

      val x = t.copy()
      t.toString mustEqual "{t (inherit=s): age=\"8\" disposition=\"hungry\" }"
      x.toString mustEqual "{t: age=\"8\" disposition=\"hungry\" name=\"Communist\" }"
    }

    "find lists" in {
      val s = new Attributes(null, "")
      s("port") = 6667
      s("hosts") = List("localhost", "skunk.example.com")
      s.getList("hosts").toList mustEqual List("localhost", "skunk.example.com")
      s.getList("non-hosts").toList mustEqual Nil
    }

    "add a nested ConfigMap" in {
      val s = new Attributes(null, "")
      val sub = new Attributes(null, "")
      s("name") = "Sparky"
      sub("name") = "Muffy"
      s.setConfigMap("dog", sub)
      s.toString mustEqual "{: dog={dog: name=\"Muffy\" } name=\"Sparky\" }"
      sub("age") = 10
      s.toString mustEqual "{: dog={dog: name=\"Muffy\" } name=\"Sparky\" }"
    }

    "toConfigString" in {
      val s = new Attributes(null, "")
      s("name") = "Sparky"
      s("age") = "10"
      s("diet") = "poor"
      s("muffy.name") = "Muffy"
      s("muffy.age") = "11"
      s("fido.name") = "Fido"
      s("fido.age") = "5"
      s("fido.roger.name") = "Roger"
      s.configMap("fido.roger").inheritFrom = Some(s.configMap("muffy"))

      val expected = """age = "10"
diet = "poor"
fido {
  age = "5"
  name = "Fido"
  roger (inherit="muffy") {
    name = "Roger"
  }
}
muffy {
  age = "11"
  name = "Muffy"
}
name = "Sparky"
"""
      s.toConfigString mustEqual expected
    }

    "copyInto" in {
      //Logger.get("").setLevel(Level.OFF)

      val s = new Attributes(null, "")
      s("name") = "Sparky"
      s("age") = "10"
      s("unused") = "nothing"
      s("longish") = "900"
      s("boolish") = "true"
      s("doublish") = "2.5"
      s("floatish") = "8.75"

      case class Person(var name: String, var age: Int, var weight: Int, var longish: Long)
      val obj = new Person("", 0, 0, 0L)
      s.copyInto(obj)
      obj mustEqual new Person("Sparky", 10, 0, 900L)

      case class Other(var boolish: Boolean, var doublish: Double, var floatish: Float)
      val obj2 = new Other(false, 0.0, 0.0f)
      s.copyInto(obj2)
      obj2 mustEqual new Other(true, 2.5, 8.75f)
    }
  }
}
