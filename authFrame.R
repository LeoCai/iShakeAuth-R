data_alice = readData()
data_bob = readData()
trainDataFromBob = getTrainData(data_alice)
trainDataFromAlice = getTrainData(data_bob)
trainParameter = train(trainDataFromBob, trainDataFromAlice)
converted_data_alice = data_alice
converted_data_bob = data_bob
bits_alice_bob = levelCrossing(converted_data_alice, converted_data_bob)
bits_alice
bits_bob
key_alice = randomnessExtract(bits_alice,codeTable)
key_bob = randomnessExtract(bits_bob,codeTable)