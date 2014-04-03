#data: http://archive.ics.uci.edu/ml/datasets/Iris


from numpy import *
import random
from numpy.core.umath import log


def is_nan(x):
    return x != x


def make_output(i, n):
    v = repeat(0, n)
    v[i] = 1
    return v


# read data from file
def read_data(file_name, class_column_idx, separator=','):
    lines = [line.strip() for line in open(file_name)]
    data_array = []
    labels = []
    input_idx = NaN
    for line in lines:
        if len(line) == 0:
            continue
        parts = line.split(separator)
        if is_nan(input_idx):
            input_idx = range(len(parts))
            input_idx.remove(class_column_idx)
        data_array.append([float(parts[i]) for i in input_idx])
        labels.append(parts[class_column_idx])
    data = array(data_array)
    labels_set = list(set(labels))
    labels_idx = [labels_set.index(x) for x in labels]
    return data, array(labels_idx), labels


# separate data into two set, with same proportion of labels
def train_test_separation(labels, train_proportion=0.8):
    if train_proportion > 1 or train_proportion < 0:
        print 'train_proportion should be in [0, 1]'
        return NaN
    labels_count = {}
    for l in labels:
        if l not in labels_count:
            labels_count[l] = 0
        labels_count[l] += 1
    for l in labels_count.iterkeys():
        labels_count[l] = int(round(labels_count[l] * train_proportion))
    train_idx = []
    for l in labels_count.iterkeys():
        l_idx = [i for i in range(len(labels)) if labels[i] == l]
        train_idx.extend(random.sample(l_idx, labels_count[l]))
    test_idx = list(set(range(len(labels))) - set(train_idx))
    return train_idx, test_idx


# http://en.wikipedia.org/wiki/Maximum_entropy_classifier
class MaxEnt:
    theta = None
    bias = None

    def __init__(self, features_count, classes_count):
        self.theta = matrix(array([random.gauss(0, 0.01) for i in range(features_count * classes_count)]).
                            reshape(classes_count, features_count))
        self.bias = matrix(array([random.gauss(0, 0.01) for i in range(classes_count)])).T

    def train(self, data, labels, max_iter=1000, tolerance=0.01, learning_rate=0.0001):
        classes_count = len(set(labels))
        if classes_count != self.theta.shape[0]:
            raise Exception("error: classes_count!")
        features_count = data.shape[1]
        if features_count != self.theta.shape[1]:
            raise Exception("error: features_count!")
        labels_m = matrix([make_output(i, classes_count) for i in labels]).T
        iteration = 1
        last_cost = inf
        while True:
            nabla_theta = matrix(repeat(0.0, prod(self.theta.shape))).reshape(self.theta.shape)

            numerators = exp(self.theta * data.T + repeat(self.bias, data.shape[0], 1))
            denumenators = repeat(numerators.sum(axis=0), numerators.shape[0], 0)
            y = numerators / denumenators  # outputs of softmax

            cost = -sum(multiply(log(y), labels_m))
            print "Iteration #" + str(iteration) + ", cost is " + str(cost)
            if iteration > max_iter:
                print "break: iterations"
                break
            if abs(last_cost - cost) < tolerance:
                print "break: tolerance"
                break
            last_cost = cost

            dE_dz = y - labels_m
            nabla_bias = dE_dz.sum(axis=1)

            for idx_data in range(data.shape[0]):
                for idx_neuron in range(self.theta.shape[0]):
                    for idx_weight in range(self.theta.shape[1]):
                        nabla_theta[idx_neuron, idx_weight] += data[idx_data, idx_weight] * dE_dz[idx_neuron, idx_data]

            self.theta -= learning_rate*nabla_theta
            self.bias -= learning_rate*nabla_bias

            iteration += 1

    # return label index or probability distribution
    def predict(self, v, return_label=True):
        v = matrix(v).T
        o = exp(self.theta * v + self.bias)
        if return_label:
            m = max(o)[0, 0]
            return [o[i, 0] for i in range(o.shape[0])].index(m)
        return o / sum(o)


if __name__ == '__main__':
    data, labels_idx, labels = read_data('./Data/iris.data.txt', 4)
    train_idx, test_idx = train_test_separation(labels_idx)
    data_train = data[train_idx, :]
    labels_train = labels_idx[train_idx]
    data_test = data[test_idx, :]
    labels_test = labels_idx[test_idx]

    m = MaxEnt(4, 3)
    m.train(data_train, labels_train, 100)

    accuracy = 0
    for i in range(len(labels_test)):
        accuracy += int(m.predict(data_test[i, :]) == labels_test[i])
    accuracy = float(accuracy)/len(labels_test)
    print "Accuracy is " + str(accuracy)