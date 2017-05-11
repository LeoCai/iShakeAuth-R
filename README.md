# 基于惯性传感器的握手认证系统

## 介绍
用户各佩戴一个安卓平台的传感器（手机或智能手表），持续采集传感器数据，对数据空间同步后，利用数据的相关性抽取一致的密钥。

此项目为系统的R语言脚本，是离线处理的系统。

## 系统设计

iShake Auth System in R language

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

<em>相关知识请参考组内云盘我的毕业论文<em>
