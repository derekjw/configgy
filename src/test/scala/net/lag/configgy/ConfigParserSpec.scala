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

import net.lag.extensions._
import org.specs2.mutable.Specification


class ConfigParserSpec extends Specification {

  class FakeImporter extends Importer {
    def importFile(filename: String, required: Boolean): String = {
      filename match {
        case "test1" =>
          "staff = \"weird skull\"\n"
        case "test2" =>
          "<inner>\n" +
          "    cat=\"meow\"\n" +
          "    include \"test3\"\n" +
          "    dog ?= \"blah\"\n" +
          "</inner>"
        case "test3" =>
          "dog=\"bark\"\n" +
          "cat ?= \"blah\"\n"
        case "test4" =>
          "cow=\"moo\"\n"
        case "test5" =>
          if (!required) "" else throw new ParseException("File not found")
      }
    }
  }

  def parse(in: String) = {
    val attr = new Config
    attr.importer = new FakeImporter
    attr.load(in)
    attr
  }


  "ConfigParser" should {
    "parse assignment" in {
      parse("weight = 48").toString mustEqual "{: weight=\"48\" }"
    }

    "parse conditional assignment" in {
      parse("weight = 48\n weight ?= 16").toString mustEqual "{: weight=\"48\" }"
    }

    "ignore comments" in {
      parse("# doing stuff\n  weight = 48\n  # more comments\n").toString mustEqual "{: weight=\"48\" }"
    }

    "parse booleans" in {
      parse("wine off\nwhiskey on\n").toString mustEqual "{: whiskey=\"true\" wine=\"false\" }"
      parse("wine = false\nwhiskey = on\n").toString mustEqual "{: whiskey=\"true\" wine=\"false\" }"
    }

    "handle nested blocks" in {
      parse("alpha=\"hello\"\n<beta>\n    gamma=23\n</beta>").toString mustEqual
        "{: alpha=\"hello\" beta={beta: gamma=\"23\" } }"
      parse("alpha=\"hello\"\n<beta>\n    gamma=23\n    toaster on\n</beta>").toString mustEqual
        "{: alpha=\"hello\" beta={beta: gamma=\"23\" toaster=\"true\" } }"
    }

    "handle nested blocks in braces" in {
      parse("alpha=\"hello\"\nbeta {\n    gamma=23\n}").toString mustEqual
        "{: alpha=\"hello\" beta={beta: gamma=\"23\" } }"
      parse("alpha=\"hello\"\nbeta {\n    gamma=23\n    toaster on\n}").toString mustEqual
        "{: alpha=\"hello\" beta={beta: gamma=\"23\" toaster=\"true\" } }"
    }

    "handle string lists" in {
      "normal" in {
        val data2 = "cats = [\"Commie\", \"Buttons\", \"Sockington\"]"
        val b = parse(data2)
        b.getList("cats").toList mustEqual List("Commie", "Buttons", "Sockington")
        b.getList("cats")(0) mustEqual "Commie"

        val data =
          "<home>\n" +
          "    states = [\"California\", \"Tennessee\", \"Idaho\"]\n" +
          "    regions = [\"pacific\", \"southeast\", \"northwest\"]\n" +
          "</home>\n"
        val a = parse(data)
        a.toString mustEqual "{: home={home: regions=[pacific,southeast,northwest] states=[California,Tennessee,Idaho] } }"
        a.getList("home.states").toList.mkString(",") mustEqual "California,Tennessee,Idaho"
        a.getList("home.states")(0) mustEqual "California"
        a.getList("home.regions")(1) mustEqual "southeast"
      }

      "without comma separators" in {
        val data2 = "cats = [\"Commie\" \"Buttons\" \"Sockington\"]"
        val b = parse(data2)
        b.getList("cats").toList mustEqual List("Commie", "Buttons", "Sockington")
        b.getList("cats")(0) mustEqual "Commie"
      }

      "with a trailing comma" in {
        val data2 = "cats = [\"Commie\", \"Buttons\", \"Sockington\",]"
        val b = parse(data2)
        b.getList("cats").toList mustEqual List("Commie", "Buttons", "Sockington")
        b.getList("cats")(0) mustEqual "Commie"
      }
    }

    "handle camelCase lists" in {
      val data =
        "<daemon>\n" +
        "    useLess = [\"one\",\"two\"]\n" +
        "</daemon>\n"
      val a = parse(data)
      a.getList("daemon.useLess").toList mustEqual List("one", "two")
    }

    "handle lists with numbers" in {
      val data = "ports = [ 9940, 9941, 9942 ]\n"
      val a = parse(data)
      a.toString mustEqual "{: ports=[9940,9941,9942] }"
      a.getList("ports").toList mustEqual List("9940", "9941", "9942")
    }

    "import files" in {
      val data1 =
        "toplevel=\"skeletor\"\n" +
        "<inner>\n" +
        "    include \"test1\"\n" +
        "    home = \"greyskull\"\n" +
        "</inner>\n"
      parse(data1).toString mustEqual "{: inner={inner: home=\"greyskull\" staff=\"weird skull\" } toplevel=\"skeletor\" }"

      val data2 =
        "toplevel=\"hat\"\n" +
        "include \"test2\"\n" +
        "include \"test4\"\n"
      parse(data2).toString mustEqual "{: cow=\"moo\" inner={inner: cat=\"meow\" dog=\"bark\" } toplevel=\"hat\" }"
    }

    "import optional files" in {
      val data1 =
        "toplevel=\"skeletor\"\n" +
        "<inner>\n" +
        "    include? \"test1\"\n" +
        "    home = \"greyskull\"\n" +
        "</inner>\n"
      parse(data1).toString mustEqual "{: inner={inner: home=\"greyskull\" staff=\"weird skull\" } toplevel=\"skeletor\" }"

      val data2 =
        "toplevel=\"hat\"\n" +
        "include? \"test2\"\n" +
        "include \"test4\"\n" +
        "include? \"test5\"\n"
      parse(data2).toString mustEqual "{: cow=\"moo\" inner={inner: cat=\"meow\" dog=\"bark\" } toplevel=\"hat\" }"
    }

    "throw an exception when importing non-existent file" in {
      val data1 = "include \"test5\"\n"
      parse(data1) must throwA(new ParseException("File not found"))
    }

    "ignore optionally imported non-existent file" in {
      val data1 = "include? \"test5\"\n"
      parse(data1).toString mustEqual "{: }"
    }

    "refuse to overload key types" in {
      val data =
        "cat = 23\n" +
        "<cat>\n" +
        "    dog = 1\n" +
        "</cat>\n"
      parse(data) must throwA(new ConfigException("Illegal key cat"))
    }

    "catch unknown block modifiers" in {
      parse("<upp name=\"fred\">\n</upp>\n") must throwA(new ParseException("Unknown block modifier"))
    }

    "handle an outer scope after a closed block" in {
      val data =
        "alpha = 17\n" +
        "<inner>\n" +
        "    name = \"foo\"\n" +
        "    <further>\n" +
        "        age = 500\n" +
        "    </further>\n" +
        "    zipcode = 99999\n" +
        "</inner>\n" +
        "beta = 19\n"
      parse(data).toString mustEqual "{: alpha=\"17\" beta=\"19\" inner={inner: further={inner.further: age=\"500\" } name=\"foo\" zipcode=\"99999\" } }"
    }

    "allow whole numbers to be identifiers" in {
      parse("1 = 2").toString mustEqual "{: 1=\"2\" }"
      parse("1 = 2\n 3 = 4").toString mustEqual "{: 1=\"2\" 3=\"4\" }"
      parse("20 = 1").toString mustEqual "{: 20=\"1\" }"
      parse("2 = \"skeletor\"").toString mustEqual "{: 2=\"skeletor\" }"
      parse("4 = \"hostname:1234\"").toString mustEqual "{: 4=\"hostname:1234\" }"
      parse("""4 = ["a", "b"]""").toString mustEqual """{: 4=[a,b] }"""
    }

    // Far longer than needed to overflow a regex that recurses per character, so the
    // check doesn't depend on the thread's stack size.
    "parse long strings without recursing to death" in {
      val plain = "x" * 100000
      parse("long = \"" + plain + "\"")("long") mustEqual plain

      val escaped = "\\\"\\u20ac\\x41\\n" * 25000
      parse("long = \"" + escaped + "\"")("long") mustEqual "\"\u20acA\n" * 25000
    }

    "parse long string lists" in {
      val plain = "x" * 100000
      parse("long = [\"" + plain + "\", \"" + plain + "\"]").getList("long").toList mustEqual List(plain, plain)
    }

    "parse long runs of comments and whitespace without recursing to death" in {
      parse("# comment\n" * 100000 + "weight = 48").toString mustEqual "{: weight=\"48\" }"
      parse("# comment\n \t\n" * 100000 + "weight = 48").toString mustEqual "{: weight=\"48\" }"
      parse("weight =" + " " * 100000 + "48").toString mustEqual "{: weight=\"48\" }"
    }

    "reject a trailing comment without a newline" in {
      parse("weight = 48\n# comment") must throwA[ParseException]
    }

    "reject malformed escapes" in {
      parse("bad = \"\\u12\"") must throwA[ParseException]
      parse("bad = \"\\x4\"") must throwA[ParseException]
      parse("bad = \"unterminated") must throwA[ParseException]
    }
  }


  "ConfigParser interpolation" should {
    "interpolate strings" in {
      parse("horse=\"ed\" word=\"sch$(horse)ule\"").toString mustEqual
        "{: horse=\"ed\" word=\"schedule\" }"
      parse("lastname=\"Columbo\" firstname=\"Bob\" fullname=\"$(firstname) $(lastname)\"").toString mustEqual
        "{: firstname=\"Bob\" fullname=\"Bob Columbo\" lastname=\"Columbo\" }"
    }

    "not interpolate unassigned strings" in {
      parse("horse=\"ed\" word=\"sch\\$(horse)ule\"").toString mustEqual "{: horse=\"ed\" word=\"sch$(horse)ule\" }"
    }

    "interpolate nested references" in {
      parse("horse=\"ed\"\n" +
            "<alpha>\n" +
            "    horse=\"frank\"\n" +
            "    drink=\"$(horse)ly\"\n" +
            "    <beta>\n" +
            "        word=\"sch$(horse)ule\"\n" +
            "        greeting=\"$(alpha.drink) yours\"\n" +
            "    </beta>\n" +
            "</alpha>").toString mustEqual
              "{: alpha={alpha: beta={alpha.beta: greeting=\"frankly yours\" word=\"schedule\" } drink=\"frankly\" horse=\"frank\" } horse=\"ed\" }"
    }

    "interpolate environment vars" in {
      parse("user=\"$(USER)\"").toString must_!=("{: user=\"$(USER)\" }")
    }

    "interpolate properties" in {
      val value = System.getProperty("java.home")
      parse("java_home=\"$(java.home)\"").toString mustEqual("{: java_home=\"" + value.quoteC + "\" }")
    }

    "properties have precedence over environment" in {
      // find an environment variable that exists
      val iter = System.getenv.entrySet.iterator
      iter.hasNext mustEqual(true)
      val entry = iter.next
      val key = entry.getKey
      val value1 = entry.getValue
      val value2 = value1 + "-test"

      val orig_prop = System.getProperty(key)
      System.clearProperty(key)

      try {
        parse("v=\"$(" + key + ")\"").toString mustEqual("{: v=\"" + value1.quoteC + "\" }")
        System.setProperty(key, value2)
        parse("v=\"$(" + key + ")\"").toString mustEqual("{: v=\"" + value2.quoteC + "\" }")
      } finally {
        if (orig_prop eq null)
          System.clearProperty(key)
        else
          System.setProperty(key, orig_prop)
      }

      parse("v=\"$(" + key + ")\"").toString mustEqual("{: v=\"" + value1.quoteC + "\" }")
    }
  }


  "ConfigParser inheritance" should {
    "inherit" in {
      val data =
        "<daemon>\n" +
        "    ulimit_fd = 32768\n" +
        "    uid = 16\n" +
        "</daemon>\n" +
        "\n" +
        "<upp inherit=\"daemon\">\n" +
        "    uid = 23\n" +
        "</upp>\n"
      val a = parse(data)
      a.toString mustEqual "{: daemon={daemon: uid=\"16\" ulimit_fd=\"32768\" } upp={upp (inherit=daemon): uid=\"23\" } }"
      a.getString("upp.ulimit_fd", "9") mustEqual "32768"
      a.getString("upp.uid", "100") mustEqual "23"
    }

    "inherit using braces" in {
      val data =
        "daemon {\n" +
        "    ulimit_fd = 32768\n" +
        "    uid = 16\n" +
        "}\n" +
        "\n" +
        "upp (inherit=\"daemon\") {\n" +
        "    uid = 23\n" +
        "}\n"
      val a = parse(data)
      a.toString mustEqual "{: daemon={daemon: uid=\"16\" ulimit_fd=\"32768\" } upp={upp (inherit=daemon): uid=\"23\" } }"
      a.getString("upp.ulimit_fd", "9") mustEqual "32768"
      a.getString("upp.uid", "100") mustEqual "23"
    }

    "use parent scope for lookups" in {
      val data =
        "<daemon><inner>\n" +
        "  <common>\n" +
        "    ulimit_fd = 32768\n" +
        "    uid = 16\n" +
        "  </common>\n" +
        "  <upp inherit=\"common\">\n" +
        "    uid = 23\n" +
        "  </upp>\n" +
        "  <slac inherit=\"daemon.inner.common\">\n" +
        "  </slac>\n" +
        "</inner></daemon>\n"
      val a = parse(data)
      a.toString mustEqual "{: daemon={daemon: inner={daemon.inner: common={daemon.inner.common: uid=\"16\" ulimit_fd=\"32768\" } " +
        "slac={daemon.inner.slac (inherit=daemon.inner.common): } upp={daemon.inner.upp (inherit=daemon.inner.common): uid=\"23\" } } } }"
      a.getString("daemon.inner.upp.ulimit_fd", "9") mustEqual "32768"
      a.getString("daemon.inner.upp.uid", "100") mustEqual "23"
      a.getString("daemon.inner.slac.uid", "100") mustEqual "16"
    }

    "handle camel case id in block" in {
      val data =
        "<daemon>\n" +
        "    useLess = 3\n" +
        "</daemon>\n"
      val exp =
        "{: daemon={daemon: useLess=\"3\" } }"
      val a = parse(data)
      a.toString mustEqual exp
      a.getString("daemon.useLess", "14") mustEqual "3"
    }

    "handle dash block" in {
      val data =
        "<daemon>\n" +
        "    <base-dat>\n" +
        "        ulimit_fd = 32768\n" +
        "    </base-dat>\n" +
        "</daemon>\n"
      val exp =
        "{: daemon={daemon: base-dat={daemon.base-dat: ulimit_fd=\"32768\" } } }"
      val a = parse(data)
      a.toString mustEqual exp
      a.getString("daemon.base-dat.ulimit_fd", "14") mustEqual "32768"
    }

    "handle camelcase block" in {
      val data =
        "<daemon>\n" +
        "    <baseDat>\n" +
        "        ulimit_fd = 32768\n" +
        "    </baseDat>\n" +
        "</daemon>\n"
      val exp =
        "{: daemon={daemon: baseDat={daemon.baseDat: ulimit_fd=\"32768\" } } }"
      val a = parse(data)
      a.toString mustEqual exp
      a.getString("daemon.baseDat.ulimit_fd", "14") mustEqual "32768"
    }

    "handle assignment after block" in {
      val data =
        "<daemon>\n" +
        "    <base>\n" +
        "        ulimit_fd = 32768\n" +
        "    </base>\n" +
        "    useless = 3\n" +
        "</daemon>\n"
      val exp =
        "{: daemon={daemon: base={daemon.base: ulimit_fd=\"32768\" } useless=\"3\" } }"
      val a = parse(data)
      a.toString mustEqual exp
      a.getString("daemon.useless", "14") mustEqual "3"
      a.getString("daemon.base.ulimit_fd", "14") mustEqual "32768"
    }

    "two consecutive groups" in {
      val data =
        "<daemon>\n" +
        "    useless = 3\n" +
        "</daemon>\n" +
        "\n" +
        "<upp inherit=\"daemon\">\n" +
        "    uid = 16\n" +
        "</upp>\n"
      val exp =
        "{: daemon={daemon: useless=\"3\" } " +
        "upp={upp (inherit=daemon): uid=\"16\" } }"
      val a = parse(data)
      a.toString mustEqual exp
      a.getString("daemon.useless", "14") mustEqual "3"
      a.getString("upp.uid", "1") mustEqual "16"
    }

    "handle a complex case" in {
      val data =
        "<daemon>\n" +
        "    useLess = 3\n" +
        "    <base-dat>\n" +
        "        ulimit_fd = 32768\n" +
        "    </base-dat>\n" +
        "</daemon>\n" +
        "\n" +
        "<upp inherit=\"daemon.base-dat\">\n" +
        "    uid = 16\n" +
        "    <alpha inherit=\"upp\">\n" +
        "        name=\"alpha\"\n" +
        "    </alpha>\n" +
        "    <beta inherit=\"daemon\">\n" +
        "        name=\"beta\"\n" +
        "    </beta>\n" +
        "    someInt=1\n" +
        "</upp>\n"
      val exp =
        "{: daemon={daemon: base-dat={daemon.base-dat: ulimit_fd=\"32768\" } useLess=\"3\" } " +
        "upp={upp (inherit=daemon.base-dat): alpha={upp.alpha (inherit=upp): name=\"alpha\" } " +
        "beta={upp.beta (inherit=daemon): name=\"beta\" } someInt=\"1\" uid=\"16\" } }"
      val a = parse(data)
      a.toString mustEqual exp
      a.getString("daemon.useLess", "14") mustEqual "3"
      a.getString("upp.uid", "1") mustEqual "16"
      a.getString("upp.ulimit_fd", "1024") mustEqual "32768"
      a.getString("upp.name", "23") mustEqual "23"
      a.getString("upp.alpha.name", "") mustEqual "alpha"
      a.getString("upp.beta.name", "") mustEqual "beta"
      a.getString("upp.alpha.ulimit_fd", "") mustEqual "32768"
      a.getString("upp.beta.useLess", "") mustEqual "3"
      a.getString("upp.alpha.useLess", "") mustEqual ""
      a.getString("upp.beta.ulimit_fd", "") mustEqual ""
      a.getString("upp.someInt", "4") mustEqual "1"
    }

    "inherit should apply explicitly" in {
      val data =
        "sanfrancisco {\n" +
        "  beer {\n" +
        "    racer5 = 10\n" +
        "  }\n" +
        "}\n" +
        "\n" +
        "atlanta (inherit=\"sanfrancisco\") {\n" +
        "  beer (inherit=\"sanfrancisco.beer\") {\n" +
        "    redbrick = 9\n" +
        "  }\n" +
        "}\n"
      val a = parse(data)
      a.configMap("sanfrancisco").inheritFrom mustEqual None
      a.configMap("sanfrancisco.beer").inheritFrom mustEqual None
      a.configMap("atlanta").inheritFrom.get.toString must find("sanfrancisco.*")
      a.configMap("atlanta.beer").inheritFrom.get.toString must find("sanfrancisco\\.beer.*")
      a.getString("sanfrancisco.beer.deathandtaxes.etc") mustEqual None
    }

    "don't choke on poundsign in strings" in {
      val data =
        "irc {\n" +
        "  channel = \"#turtle\"\n" +
        "  server = \"irc.example.com\"\n" +
        "  username = \"user\"\n" +
        "  password = \"pass\"\n" +
        "}\n"
      val a = parse(data)
      a("irc.channel") mustEqual "#turtle"
      a("irc.server") mustEqual "irc.example.com"
    }

    "read a really long list of strings without recursing to death" in {
      //skip("not fixed yet")
      val c = new Config
      c.importer = new ResourceImporter(getClass.getClassLoader)
      c.load("include \"evil.conf\"\n")
      c.getList("things").size mustEqual 1008
    }
  }
}
