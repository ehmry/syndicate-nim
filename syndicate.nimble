# Package

version = "20230326"
author        = "Emery Hemingway"
description   = "Syndicated actors for conversational concurrency"
license       = "Unlicense"
srcDir        = "src"
bin           = @["syndicate/unix/swaybar_status_actor"]


# Dependencies

requires "nim >= 1.4.8", "nimSHA2 >= 0.1.1", "preserves >= 20221208", "taps >= 20221119"
