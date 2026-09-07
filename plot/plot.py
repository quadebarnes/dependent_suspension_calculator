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

    ax.plot(travel, brakeVals, label="Braking Anti", color="tab:blue")
    ax.plot(travel, accelVals, label="Acceleration Anti", color="tab:orange")

    restingIndex = travel.index(0)
    startX = travel[0]
    endX = travel[-1]
    brakeRestingVal = brakeVals[restingIndex]
    brakeStartVal = brakeVals[0]
    brakeEndVal = brakeVals[-1]
    accelRestingVal = accelVals[restingIndex]
    accelStartVal = accelVals[0]
    accelEndVal = accelVals[-1]

    ax.plot(0, brakeRestingVal, "o", ms=4, color="tab:blue")
    ax.plot(startX, brakeStartVal, "o", ms=4, color="tab:blue")
    ax.plot(endX, brakeEndVal, "o", ms=4, color="tab:blue")
    ax.plot(0, accelRestingVal, "o", ms=4, color="tab:orange")
    ax.plot(startX, accelStartVal, "o", ms=4, color="tab:orange")
    ax.plot(endX, accelEndVal, "o", ms=4, color="tab:orange")

    ax.annotate(
        f"{brakeRestingVal:.2f}%",
        (0, brakeRestingVal),
        textcoords="offset points",
        xytext=(9, 14),
        fontsize=9,
    )
    ax.annotate(
        f"{brakeStartVal:.2f}%",
        (startX, brakeStartVal),
        textcoords="offset points",
        xytext=(9, 14),
        fontsize=9,
    )
    ax.annotate(
        f"{brakeEndVal:.2f}%",
        (endX, brakeEndVal),
        textcoords="offset points",
        xytext=(9, 14),
        fontsize=9,
    )
    ax.annotate(
        f"{accelRestingVal:.2f}%",
        (0, accelRestingVal),
        textcoords="offset points",
        xytext=(9, 14),
        fontsize=9,
    )
    ax.annotate(
        f"{accelStartVal:.2f}%",
        (startX, accelStartVal),
        textcoords="offset points",
        xytext=(9, 14),
        fontsize=9,
    )
    ax.annotate(
        f"{accelEndVal:.2f}%",
        (endX, accelEndVal),
        textcoords="offset points",
        xytext=(9, 14),
        fontsize=9,
    )

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
