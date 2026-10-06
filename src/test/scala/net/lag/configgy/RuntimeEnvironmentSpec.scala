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

import _root_.java.io._
import _root_.org.specs2.mutable.Specification
import net.lag.TestHelper


class RuntimeEnvironmentSpec extends Specification with TestHelper {
  // Examples share the global Configgy.config.
  sequential

  private def writeConfigFile(filename: String, data: String) = {
    val f = new FileOutputStream(folderName + "/" + filename)
    f.write(data.getBytes)
    f.close
  }


  "RuntimeEnvironment" should {
    "allow -D options to override the config file" in {
      withTempFolder {
        val data1 =
          "name=\"Nibbler\"\n" +
          "\n" +
          "<nested>\n" +
          "    level=\"WARNING\"\n" +
          "</nested>\n"
        writeConfigFile("test.conf", data1)

        val runtime = new RuntimeEnvironment(classOf[Config])
        runtime.load(Array("-f", folderName + "/test.conf", "-Dname=Bender", "-D", "nested.extra=tasty"))

        Configgy.config("name") mustEqual "Bender"
        Configgy.config("nested.level") mustEqual "WARNING"
        Configgy.config("nested.extra") mustEqual "tasty"
      }
    }

    "find executable jar path" in {
      val runtime = new RuntimeEnvironment(classOf[Config])
      runtime.findCandidateJar(List("./dist/flockdb/flockdb-1.4.1.jar"), "flockdb", "1.4.1") mustEqual
        Some("./dist/flockdb/flockdb-1.4.1.jar")
      runtime.findCandidateJar(List("./dist/flockdb/flockdb_2.7.7-1.4.1.jar"), "flockdb", "1.4.1") mustEqual
        Some("./dist/flockdb/flockdb_2.7.7-1.4.1.jar")
      runtime.findCandidateJar(List("./dist/flockdb/wrong-1.4.1.jar"), "flockdb", "1.4.1") mustEqual
        None
      runtime.findCandidateJar(List("./dist/flockdb/flockdb-1.4.1-SNAPSHOT.jar"), "flockdb", "1.4.1-SNAPSHOT") mustEqual
        Some("./dist/flockdb/flockdb-1.4.1-SNAPSHOT.jar")
    }
  }
}
