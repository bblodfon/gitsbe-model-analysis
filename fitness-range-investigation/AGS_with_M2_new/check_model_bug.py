from os import listdir
from os.path import isfile, join

models = [model for model in listdir("./models") if isfile(join("./models", model))]
#print(len(models))

myfile = open('model_predictions').read();
#print(myfile)

for model in models:
	if model not in myfile:
		print('The model %s is not found' %model)
