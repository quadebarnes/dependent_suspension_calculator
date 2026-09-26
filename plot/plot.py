import csv
import sys

import matplotlib
import matplotlib.pyplot as plt

matplotlib.use("Agg")


def args_valid(args):
    return len(args) == 3


def get_data(path):
    try:
        with open(path) as file:
            return [row for row in csv.DictReader(file)]
    except FileNotFoundError as e:
        print(e)


def plot_antis(data, out_path):
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

    (part1, part2) = out_path.split("/")
    fig.savefig(f"{part1}/anits_{part2}")


def plot_pinion_angle_change(data, out_path):
    travel = [float(row["Travel"]) for row in data]
    pinion_angle_chng = [float(row["Pinion Angle Change"]) for row in data]

    fig, ax = plt.subplots()

    ax.plot(travel, pinion_angle_chng, label="Pinion Angle Change", color="tab:blue")

    restingIndex = travel.index(0)
    startX = travel[0]
    endX = travel[-1]
    pinion_resting_val = pinion_angle_chng[restingIndex]
    pinion_start_val = pinion_angle_chng[0]
    pinion_end_val = pinion_angle_chng[-1]

    ax.plot(0, pinion_resting_val, "o", ms=4, color="tab:blue")
    ax.plot(startX, pinion_start_val, "o", ms=4, color="tab:blue")
    ax.plot(endX, pinion_end_val, "o", ms=4, color="tab:blue")

    ax.annotate(
        f"{pinion_resting_val:.2f} deg",
        (0, pinion_resting_val),
        textcoords="offset points",
        xytext=(9, 14),
        fontsize=9,
    )
    ax.annotate(
        f"{pinion_start_val:.2f} deg",
        (startX, pinion_start_val),
        textcoords="offset points",
        xytext=(9, 14),
        fontsize=9,
    )
    ax.annotate(
        f"{pinion_end_val:.2f} deg",
        (endX, pinion_end_val),
        textcoords="offset points",
        xytext=(9, 14),
        fontsize=9,
    )

    plt.title("Pinion Angle Change Across Travel")
    plt.legend()
    plt.grid(True)

    ax.set_xlabel("Axle Travel (inches)")
    ax.set_ylabel("Pinion Angle Change from Resting (deg)")

    (part1, part2) = out_path.split("/")
    fig.savefig(f"{part1}/pinion_angle_{part2}")


def main():
    if args_valid(sys.argv):
        in_path = sys.argv[1]
        out_path = sys.argv[2]

        data = get_data(in_path)
        plot_antis(data, out_path)
        plot_pinion_angle_change(data, out_path)
    else:
        print("Usage: python plot.py <data.csv> <output_path.png>")


if __name__ == "__main__":
    main()
