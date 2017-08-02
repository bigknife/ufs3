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
    ```

2. 初始化文件系统
    建立一个大文件会花费较长时间，一般在系统启动前初始化文件系统：

    ```
     ~/W/tmp ./ufs3 init -f ./filler_1.filler -bs 100G -is 2G
    ```
    等待操作系统刷入缓存，文件系统初始化完毕，可能花费较长时间（100G大概在6分钟左右）

3. 存储文件
    通过`put`命令可将一个本地文件推入`UFS3`文件系统

    ```
    ufs3 put -f ~/Downloads/neo4j-community_macos_3_2_1.dmg -ll error
    ```

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
    通过`http-server`命令可启动一个`restful webservice`，提供 `get/{key}` 和 `put/{key}` 端点进行文件的存取.

    ```
    ufs3 serve --host localhost --port 3081 -ll info
    ```

8. 启动备份服务器
    通过`backup-server`命令，启动一个备份服务器，备份服务器仅接受一个tcp连接作为备份使用。
    ```
    ufs3 backup-server --host 0.0.0.0 --port 3081 -ll debug
    ```

    为了调整jvm内存等参数，建议使用直接使用java命令的启动方式：（加入ufs的目录为/opt/ufs3/ufs3）:
    ```
    java -server -Xms4G -Xmx6G -jar /opt/ufs3/ufs3 backup-server --host 0.0.0.0 --port 3081 -ll debug
    ```

## Evniroments

1. 测试环境：
    主节点：10.65.209.37:3080 - 备份节点：10.65.209.38:3081
    主节点：10.65.209.39:3080 - 备份节点：10.65.209.150:3081

## Contribute
1. 欢迎提各种`Bug`,`Feature`, 请移步[https://gitlab.barcsys.com/bigknife/ufs3/issues](https://gitlab.barcsys.com/bigknife/ufs3/issues)
2. 欢迎`fork`: [https://gitlab.barcsys.com/bigknife/ufs3](https://gitlab.barcsys.com/bigknife/ufs3)

## Contributor
* [宋增辉](https://gitlab.barcsys.com/u/bigknife)
* [宋文超](https://gitlab.barcsys.com/u/song)