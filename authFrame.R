source("./readData.R")
source("./train.R")

file_alice = "./datas/alice.csv"
file_bob = "./datas/bob.csv"
data_alice = readData(file_alice)
data_bob = readData(file_bob)
data_timealigned = cross_correlation(data_alice,data_bob)
trainDataFromBob = getTrainData(data_alice)
trainDataFromAlice = getTrainData(data_bob)
trainParameter = train(trainDataFromBob, trainDataFromAlice)
converted_data_alice = convertData(data_alice,trainParameter$initTheta1)
converted_data_bob = convertData(data_bob,trainParameter$initTheta2)
bits_alice_bob = levelCrossing(converted_data_alice, converted_data_bob)
bits_alice
bits_bob
bits_reconcilation = reconcilation(bits_alice,bits_bob)
key = randomnessExtract(bits_reconcilation,codeTable)