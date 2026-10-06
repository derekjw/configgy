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

import _root_.java.io.{File, FileOutputStream}
import _root_.java.lang.management.ManagementFactory
import _root_.javax.{management => jmx}
import _root_.net.lag.TestHelper
import _root_.org.specs2.mutable.Specification


class ConfigSpec extends Specification with TestHelper {

  class FakeSubscriber extends Subscriber {
    def validate(current: Option[ConfigMap], replacement: Option[ConfigMap]): Unit = { }
    def commit(current: Option[ConfigMap], replacement: Option[ConfigMap]): Unit = { }
  }

  // remembers the before & after config nodes when committing a change
  class MemorySubscriber extends Subscriber {
    var used = false
    var savedCurrent: Option[ConfigMap] = None
    var savedReplacement: Option[ConfigMap] = None

    def validate(current: Option[ConfigMap], replacement: Option[ConfigMap]): Unit = { }
    def commit(current: Option[ConfigMap], replacement: Option[ConfigMap]): Unit = {
      used = true
      savedCurrent = current match {
        case None => None
        case Some(x) => Some(x.asInstanceOf[Attributes].copy)
      }
      savedReplacement = replacement
    }
  }


  // refuses any change to its node.
  class AngrySubscriber extends Subscriber {
    def validate(current: Option[ConfigMap], replacement: Option[ConfigMap]): Unit = throw new ValidationException("no way!")
    def commit(current: Option[ConfigMap], replacement: Option[ConfigMap]): Unit = { }
  }


  "Config" should {
    "take subscriptions" in {
      val c = new Config
      var id = c.subscribe("alpha.beta.gamma", new FakeSubscriber)

      c.debugSubscribers mustEqual "subs=0 { alpha=0 { beta=0 { gamma=1 } } }"
      c.unsubscribe(id)
      c.debugSubscribers mustEqual "subs=0 { alpha=0 { beta=0 { gamma=0 } } }"
      id = c.subscribe("alpha.beta") { (attr: Option[ConfigMap]) => Console.println("hello") }
      c.debugSubscribers mustEqual "subs=0 { alpha=0 { beta=1 { gamma=0 } } }"
      c.unsubscribe(id)
      c.debugSubscribers mustEqual "subs=0 { alpha=0 { beta=0 { gamma=0 } } }"
    }

    "call subscribers" in {
      val c = new Config
      c("alpha.beta.gamma") = "hello"

      var checked = false
      c.subscribe("alpha.beta") { (attr: Option[ConfigMap]) => checked = true }
      checked must beFalse
      c("alpha.beta.delta") = "goodbye"
      checked must beTrue
    }

    "call subscribers with the old & new data" in {
      val c = new Config
      c("alpha.beta.gamma") = "hello"

      val sub = new MemorySubscriber
      c.subscribe("alpha.beta", sub)
      sub.used must beFalse

      c("alpha.beta.delta") = "goodbye"
      sub.used must beTrue
      sub.savedCurrent.get.toString mustEqual "{alpha.beta: gamma=\"hello\" }"
      sub.savedReplacement.get.toString mustEqual "{alpha.beta: delta=\"goodbye\" gamma=\"hello\" }"
      c.toString mustEqual "{: alpha={alpha: beta={alpha.beta: delta=\"goodbye\" gamma=\"hello\" } } }"

      c("alpha.beta.gamma") = "gutentag"
      sub.savedCurrent.get.toString mustEqual "{alpha.beta: delta=\"goodbye\" gamma=\"hello\" }"
      sub.savedReplacement.get.toString mustEqual "{alpha.beta: delta=\"goodbye\" gamma=\"gutentag\" }"
      c.toString mustEqual "{: alpha={alpha: beta={alpha.beta: delta=\"goodbye\" gamma=\"gutentag\" } } }"
    }

    "abort a rejected change" in {
      val c = new Config
      c("alpha.beta.gamma") = "hello"

      c.subscribe("alpha.beta", new AngrySubscriber)
      (c("alpha.beta.gamma") = "gutentag") must throwA(new ValidationException("no way!"))
      c("alpha.giraffe") = "tall!"
      c.toString mustEqual "{: alpha={alpha: beta={alpha.beta: gamma=\"hello\" } giraffe=\"tall!\" } }"
    }

    "track changes to a ConfigMap that's tacked on" in {
      val c = new Config
      val sub = new MemorySubscriber
      c.subscribe(sub)

      val hostsConfig = new Config
      c.setConfigMap("hosts", hostsConfig)
      sub.used mustEqual true
      sub.used = false

      c.getConfigMap("hosts").get.setString("localhost", "awesome")
      sub.used mustEqual true
    }

    "deal correctly with multiple subscribers at different nodes" in {
      val c = new Config
      c("alpha.beta.gamma") = "hello"
      c("alpha.giraffe") = "tall!"
      c("forest.fires.are") = "bad"

      val rootsub = new MemorySubscriber
      c.subscribe(rootsub)
      val firesub = new AngrySubscriber
      c.subscribe("forest.fires", firesub)
      val betasub = new MemorySubscriber
      c.subscribe("alpha.beta", betasub)

      c("unrelated") = 39
      rootsub.used must beTrue
      betasub.used must beFalse
      rootsub.savedCurrent.get.toString mustEqual
        "{: alpha={alpha: beta={alpha.beta: gamma=\"hello\" } giraffe=\"tall!\" } forest={forest: fires={forest.fires: are=\"bad\" } } }"
      rootsub.savedReplacement.get.toString mustEqual
        "{: alpha={alpha: beta={alpha.beta: gamma=\"hello\" } giraffe=\"tall!\" } forest={forest: fires={forest.fires: are=\"bad\" } } unrelated=\"39\" }"

      rootsub.used = false
      c("forest.matches") = false
      rootsub.used must beTrue
      betasub.used must beFalse
      c.getConfigMap("forest").get.toString mustEqual
        "{forest: fires={forest.fires: are=\"bad\" } matches=\"false\" }"

      c.remove("forest") must throwA(new ValidationException("no way!"))

      rootsub.used = false
      betasub.used = false
      c("alpha.beta.gamma") = "goodbye"
      rootsub.used must beTrue
      betasub.used must beTrue
      betasub.savedCurrent.get.toString mustEqual "{alpha.beta: gamma=\"hello\" }"
      betasub.savedReplacement.get.toString mustEqual "{alpha.beta: gamma=\"goodbye\" }"
    }

    "include relative files" in {
      withTempFolder {
        val inner = new File(folderName, "inner")
        inner.mkdir

        val data1 = "fruit = 17\ninclude \"inner/punch.conf\"\n"
        val f1 = new FileOutputStream(new File(folderName, "fruit.conf"))
        f1.write(data1.getBytes)
        f1.close
        val data2 = "punch = 23\n"
        val f2 = new FileOutputStream(new File(inner, "punch.conf"))
        f2.write(data2.getBytes)
        f2.close

        val c = new Config
        c.loadFile(folderName, "fruit.conf")
        c.toString mustEqual "{: fruit=\"17\" punch=\"23\" }"
      }
    }

    "load a test resource as a sanity check" in {
      getClass.getClassLoader.getResource("happy.conf") must not(beNull)
    }

    "include from a resource" in {
      val c = new Config
      c.importer = new ResourceImporter(getClass.getClassLoader)
      c.load("include \"happy.conf\"\n")
      c.toString mustEqual "{: commie=\"501\" }"
    }

    "build from a map" in {
      val c = Config.fromMap(Map("apples" -> "23", "oranges" -> "17", "fruit.misc" -> "x,y,z"))
      c("apples") mustEqual "23"
      c("oranges") mustEqual "17"
      c("fruit.misc") mustEqual "x,y,z"
      c.toString mustEqual "{: apples=\"23\" fruit={fruit: misc=\"x,y,z\" } oranges=\"17\" }"
      c.configMap("fruit").getName mustEqual "fruit"
    }

    "register jmx" in {
      val c = Config.fromMap(Map("apples" -> "23", "oranges" -> "17", "fruit.misc" -> "x,y,z"))
      c.registerWithJmx("com.example.test")
      val mbs = ManagementFactory.getPlatformMBeanServer()
      mbs.getAttribute(new jmx.ObjectName("com.example.test:type=Config,name=(root)"), "apples") mustEqual "23"
      mbs.getAttribute(new jmx.ObjectName("com.example.test:type=Config,name=(root)"), "oranges") mustEqual "17"
      mbs.getAttribute(new jmx.ObjectName("com.example.test:type=Config,name=fruit"), "misc") mustEqual "x,y,z"
    }

    "reload from string" in {
      val c = Config.fromString("""apples="23" oranges="17" basket { apples = true oranges = false }""")
      c("apples") mustEqual "23"
      c("oranges") mustEqual "17"
      c("basket.apples", false) mustEqual true
      c("basket.oranges", false) mustEqual false
      c.setString("apples", "red")
      c.configMap("basket").setBool("apples", false)
      c("apples") mustEqual "red"
      c("basket.apples", false) mustEqual false
      c.reload()
      c("apples") mustEqual "23"
      c("basket.apples", false) mustEqual true
    }

    "reload from a file" in {
      val c = Config.fromResource("happy.conf", getClass.getClassLoader)
      c.getInt("commie") mustEqual Some(501)
      c.setInt("commie", 401)
      c.getInt("commie") mustEqual Some(401)
      c.reload()
      c.getInt("commie") mustEqual Some(501)
    }
  }
}
