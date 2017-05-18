# 基于惯性传感器的握手认证系统

## 介绍
用户各佩戴一个安卓平台的传感器（手机或智能手表），持续采集传感器数据，对数据空间同步后，利用数据的相关性抽取一致的密钥。

此项目为系统的R语言脚本，是离线处理的系统。

## 系统设计

### 系统模块设计图
<img src="https://github.com/LeoCai/iShakeAuth-R/blob/master/imgs/system_design.png" width = "600" alt="系统设计" align=center />
 
### 系统交互图
<img src="https://github.com/LeoCai/iShakeAuth-R/blob/master/imgs/system_process.png" width = "600" alt="系统交互" align=center />

### 系统数据转换算法图
<img src="https://github.com/LeoCai/iShakeAuth-R/blob/master/imgs/system_alg.png" width = "600" alt="系统核心" align=center />


## 系统流程
With iShake, Two people can naturally build trust via physical handshake.

1. Read sensor data;

2. Align data using cross correlation technology;

3. Send Bob's head Datas to Alice for training use;

4. Alice adjust Bob's coordinate system to the same coordinate system;

5. Alice send back adjust parameter back to Bob;

6. Bob use the parameter to adjust coordinate system;

7. Alice and Bob coorperate to generate bits using level crossing technnlogy;

8. Alice and Bob reconcilate to get the same bits;

9. Alice and Bob extract randomness using malkov modle.

## 重要参考文献
1. Xiaojun Zhu 的论文《Extracting Secret key from Wireless Link Dynamics in Vehicular Environments》
2. 《An introduction to inertial navigation》 21页
3. LeoCai 的毕业论文《基于惯性传感器的人体运动感知机制研究》
4. LeoCai 组内云盘的ppt

<em>相关知识请参考组内云盘我的毕业论文<em>
