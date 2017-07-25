# UFS3 - to store massive small files

## Compile
1. 下载源码
    ```
    git clone https://gitlab.barcsys.com/bigknife/ufs3.git
    ```
2. SBT编译
    ```
    sbt "project integration" assembly
    ```
3. 加入系统命令路径
    ```
    ln -s $UFS_SRC_HOME//integration/target/scala-2.11/ufs3 /usr/local/bin/ufs3
    ```

## Usage
1. 查看帮助：
    ```
    ufs3 -h

    ufs3 0.1
    Usage: ufs3 [init|put|get|list|free|serve] [options]

      -v, --version            print version of current ufs3
      -h, --help               prints this usage text
    Command: init [options]
    init: create a block file for ufs3
      -bs, --block-size <value>
                               the file block size, should end with G, M, K as the unit
      -f, --file <value>       the block file path, default is current ./ufs3.filler
      -is, --idx-size <value>  the block index file size, should end with G, M, K as the unit
      -ll, --log-level <value>
                               log level: debug|info|warn|error, if not same as this, it should be the default: debug
    Command: put [options]
    put: put a local file or some remote resource identified by url into the ufs3
      -f, --file <value>       local file path, should be a valid local file
      -o, --out <value>        the block file path, default is current ./ufs3.filler
      -ll, --log-level <value>
                               log level: debug|info|warn|error, if not same as this, it should be the default: debug
    Command: get [options]
    get: read file from ufs3 with key, and output to somewhere
      -k, --key <value>        the file key in the ufs3
      -f, --file <value>       local file path, should be a valid local file
      -i, --in <value>         the block file path, default is current ./ufs3.filler
      -ll, --log-level <value>
                               log level: debug|info|warn|error, if not same as this, it should be the default: debug
    Command: list [options]
    list: list the file in ufs3 instance
      --limit <value>          limit the count of result
      --order <value>          the list result order
      -f, --file <value>       the block file path, default is current ./ufs3.filler
      -ll, --log-level <value>
                               log level: debug|info|warn|error, if not same as this, it should be the default: debug
    Command: free [block|idx]
    free: view the free space
    Command: free block [options]
    block: view the free space of block
      --with-unit <value>      the unit for showing free space (G|M|K), default is M
      -f, --file <value>       the block file path, default is current ./ufs3.filler
      -ll, --log-level <value>
                               log level: debug|info|warn|error, if not same as this, it should be the default: debug
    Command: free idx [options]
    idx: view the free space of index
      --with-unit <value>      the unit for showing free space (G|M|K), default is M
      -f, --file <value>       the block file path, default is current ./ufs3.filler
      -ll, --log-level <value>
                               log level: debug|info|warn|error, if not same as this, it should be the default: debug
    Command: serve [options]
    serve: start a http server to expose get/put interface
      --host <value>           the host to be listened
      --port <value>           the port to be listened
      -f, --file <value>       the block file path, default is current ./ufs3.filler
      -ll, --log-level <value>
                               log level: debug|info|warn|error, if not same as this, it should be the default: debug
    ```

2. 初始化文件系统
    建立一个大文件会花费较长时间，一般在系统启动前初始化文件系统：

    ```
     ~/W/tmp ./ufs3 init -f ./filler_1.filler -bs 100G -is 2G

    2017-07-25 16:22:00,420 [INFO|ufs3|Main$] It will take a long time to init a ufs3 block file, please wait with your patience!
    2017-07-25 16:22:00,496 [DEBUG|ufs3|LogInterpreter$$anonfun$debug$1$$anonfun$apply$1] existing /Users/bigknife/Working/tmp/./filler_1.filler ?
    2017-07-25 16:22:00,499 [DEBUG|ufs3|LogInterpreter$$anonfun$debug$1$$anonfun$apply$1] not existed /Users/bigknife/Working/tmp/./filler_1.filler
    2017-07-25 16:22:00,500 [DEBUG|ufs3|LogInterpreter$$anonfun$debug$1$$anonfun$apply$1] creating /Users/bigknife/Working/tmp/./filler_1.filler
    2017-07-25 16:22:02,004 [DEBUG|ufs3|LogInterpreter$$anonfun$debug$1$$anonfun$apply$1] created /Users/bigknife/Working/tmp/./filler_1.filler, size=ufs3.kernel.block.Block$Size$SizeOp$$anon$6@7995092a
    2017-07-25 16:22:02,005 [DEBUG|ufs3|LogInterpreter$$anonfun$debug$1$$anonfun$apply$1] creating index for /Users/bigknife/Working/tmp/./filler_1.filler
    2017-07-25 16:22:02,030 [DEBUG|ufs3|LogInterpreter$$anonfun$debug$1$$anonfun$apply$1] created index for /Users/bigknife/Working/tmp/./filler_1.filler
    2017-07-25 16:22:02,030 [DEBUG|ufs3|LogInterpreter$$anonfun$debug$1$$anonfun$apply$1] initializing filler file: /Users/bigknife/Working/tmp/./filler_1.filler
    2017-07-25 16:22:02,044 [DEBUG|ufs3|LogInterpreter$$anonfun$debug$1$$anonfun$apply$1] initialized filler file: /Users/bigknife/Working/tmp/./filler_1.filler
    2017-07-25 16:22:02,045 [DEBUG|ufs3|LogInterpreter$$anonfun$debug$1$$anonfun$apply$1] init filler index file for /Users/bigknife/Working/tmp/./filler_1.filler
    2017-07-25 16:22:02,050 [DEBUG|ufs3|LogInterpreter$$anonfun$debug$1$$anonfun$apply$1] lock file: /Users/bigknife/Working/tmp/./filler_1.filler
    2017-07-25 16:22:02,054 [DEBUG|ufs3|LogInterpreter$$anonfun$debug$1$$anonfun$apply$1] close filler file
    2017-07-25 16:22:02,059 [DEBUG|ufs3|LogInterpreter$$anonfun$debug$1$$anonfun$apply$1] close fildex file
    2017-07-25 16:22:02,060 [DEBUG|ufs3|LogInterpreter$$anonfun$debug$1$$anonfun$apply$1] unlock block file
    2017-07-25 16:22:02,061 [DEBUG|ufs3|LogInterpreter$$anonfun$debug$1$$anonfun$apply$1] close block file
    2017-07-25 16:22:02,062 [INFO|ufs3|Main$] Congratulations! the block file is created, see /Users/bigknife/Working/tmp/./filler_1.filler
    2017-07-25 16:22:02,063 [INFO|ufs3|Main$] Now, Give more patience, wait the OS refresh the buffer to the disk, until this process exist, PLEASE DON'T send kill signal to this process
    ```
    等待操作系统刷入缓存，文件系统初始化完毕，可能花费较长时间（100G大概在6分钟左右）

    `init` 参数：

    1. `--filler-file`, 简写为`-f`, 指定文件路径
    2. `--block-size`, 简写为`-bs`, 指定文件大小，支持`G`,`M`,`K`为单位，如 `-bs 100G`
    3. `--index-size`, 简写为`-is`, 指定索引文件大小，支持`G`,`M`,`K`为单位，如 `-is 100G`, 每条索引占`48`字节，可根据实际情况，估算索引的合适大小

3. 存储文件
    通过`put`命令可将一个本地文件推入`UFS3`文件系统

    ```
    ufs3 put -f ~/Downloads/neo4j-community_macos_3_2_1.dmg -ll error
    ```
    todo write more...

4. 读取文件
    通过`get`命令可从`UFS3`中读取文件（指定key）

    ```
    ufs3 get -k 260210c32164b7cef5378f7e9c638b23 -f a.jpg
    ```
    todo write more...

5. 查看块文件剩余空间

    ```
    ufs3 free block --with-unit G
    ```
6. 查看索引文件剩余空间

    ```
    ufs3 free idx --with-unit G
    ```
7. 启动服务接口
    通过`serve`命令可启动一个`restful webservice`，提供 `get/{key}` 和 `put/{key}` 端点进行文件的存取.

    ```
    ufs3 serve --host localhost --port 3081 -ll info

    16:34:31.594 [SimplePharaohApp-akka.actor.default-dispatcher-3] INFO akka.event.slf4j.Slf4jLogger - Slf4jLogger started

        __  __________________
      / / / / ____/ ___/__  /
     / / / / /_   \__ \ /_ <
    / /_/ / __/  ___/ /__/ /
    \____/_/    /____/____/
    ```

## Contribute
1. 欢迎提各种`Bug`,`Feature`, 请移步[https://gitlab.barcsys.com/bigknife/ufs3/issues](https://gitlab.barcsys.com/bigknife/ufs3/issues)
2. 欢迎`fork`: [https://gitlab.barcsys.com/bigknife/ufs3](https://gitlab.barcsys.com/bigknife/ufs3)

## Contributor
* [宋增辉](https://gitlab.barcsys.com/u/bigknife)
* [宋文超](https://gitlab.barcsys.com/u/song)