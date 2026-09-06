import csv
import sys

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt


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
    armAngleVals = [row["Lower Arm Angle"] for row in data]
    brakeVals = [row["Braking Anti"] for row in data]
    accelVals = [row["Acceleration Anti"] for row in data]

    plt.plot(armAngleVals, brakeVals, label="Braking Anti")
    plt.plot(armAngleVals, accelVals, label="Acceleration Anti")
    plt.savefig(outPath)


def main():
    if argsValid(sys.argv):
        inPath = sys.argv[1]
        outPath = sys.argv[2]

        data = getData(inPath)
        plotAntis(data, outPath)


if __name__ == "__main__":
    main()
