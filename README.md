%---------------------------------------<br/>
% Name      :   ResourceManager         <br/>
% Author    :   Chris                   <br/>
% Version   :   0.1                     <br/>
% Email     :   cablsbs@live.com        <br/>
%---------------------------------------<br/>

说明:<br/>
这是一个用Erlang实现的版本管理工具(参考了9mobile/ResourceManager)，这个工具需要在工作机上安装erlang.<br/>

使用方法：<br/>
    进入sh目录，<br/>
    执行 sh make.sh进行编译<br/>
    执行 sh gen.sh 版本库目录 新版本资源目录 版本号<br/>
    (注：首次进行版本生成时，选择一个空目录作为版本库目录;<br/>
         版本号为浮点数，小数点后2位数字有效，默认版本号为旧版本号 + 0.10;<br/>
         首次生成版本时，输入版本号需不小于0.00！）<br/>
