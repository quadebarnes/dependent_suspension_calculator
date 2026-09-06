import csv
import sys

import matplotlib.pyplot as plt

# import matplotlib
# matplotlib.use("Agg")
from matplotlib.ticker import MultipleLocator


def validPath(path):
    try:
        open(path)
    except FileNotFoundError as e:
        print(e)


def argsValid(args):
    if len(args) == 3:
        return map(validPath, args[1:])


def getData(path):
    with open(path) as file:
        return [row for row in csv.DictReader(file)]


def plotAntis(data, outPath):
    travel = [float(row["Travel"]) for row in data]
    brakeVals = [float(row["Braking Anti"]) for row in data]
    accelVals = [float(row["Acceleration Anti"]) for row in data]

    fig, ax = plt.subplots()

    ax.plot(travel, brakeVals, label="Braking Anti")
    ax.plot(travel, accelVals, label="Acceleration Anti")

    plt.title("Anti Values Across Travel")
    plt.legend()
    plt.grid(True)

    ax.set_xlabel("Axle Travel (inches)")
    ax.set_ylabel("Anti Value (%)")

    ax.axhspan(50, 65, alpha=0.1)

    fig.savefig(outPath)
    # plt.show()


def main():
    if argsValid(sys.argv):
        inPath = sys.argv[1]
        outPath = sys.argv[2]

        data = getData(inPath)
        plotAntis(data, outPath)


if __name__ == "__main__":
    main()
