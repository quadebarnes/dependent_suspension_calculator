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


def list_data(data):
    front_travel = []
    rear_travel = []
    front_pinion_angle = []
    rear_pinion_angle = []
    anti_squat = []
    anti_rise = []
    anti_lift = []
    anti_dive = []

    for line in data:
        front_travel.append(float(line["Front Travel"]))
        rear_travel.append(float(line["Rear Travel"]))
        front_pinion_angle.append(float(line["Front Pinion Angle Change"]))
        rear_pinion_angle.append(float(line["Rear Pinion Angle Change"]))
        anti_squat.append(float(line["Anti-squat"]))
        anti_rise.append(float(line["Anti-rise"]))
        anti_lift.append(float(line["Anti-lift"]))
        anti_dive.append(float(line["Anti-dive"]))

    return (
        front_travel,
        rear_travel,
        front_pinion_angle,
        rear_pinion_angle,
        anti_squat,
        anti_rise,
        anti_lift,
        anti_dive,
    )


def plot_anti(axes, plot_title, travel, anti1_name, anti1, anti2_name, anti2):
    axes.plot(travel, anti1, label=anti1_name, color="tab:blue")
    axes.plot(travel, anti2, label=anti2_name, color="tab:orange")

    restingIndex = travel.index(0)
    startX = travel[0]
    endX = travel[-1]
    anti1RestingVal = anti1[restingIndex]
    anti1StartVal = anti1[0]
    anti1EndVal = anti1[-1]
    anti2RestingVal = anti2[restingIndex]
    anti2StartVal = anti2[0]
    anti2EndVal = anti2[-1]

    axes.plot(0, anti1RestingVal, "o", ms=4, color="tab:blue")
    axes.plot(startX, anti1StartVal, "o", ms=4, color="tab:blue")
    axes.plot(endX, anti1EndVal, "o", ms=4, color="tab:blue")
    axes.plot(0, anti2RestingVal, "o", ms=4, color="tab:orange")
    axes.plot(startX, anti2StartVal, "o", ms=4, color="tab:orange")
    axes.plot(endX, anti2EndVal, "o", ms=4, color="tab:orange")

    axes.annotate(
        f"{anti1RestingVal:.2f}%",
        (0, anti1RestingVal),
        textcoords="offset points",
        xytext=(9, 14),
        fontsize=9,
    )
    axes.annotate(
        f"{anti1StartVal:.2f}%",
        (startX, anti1StartVal),
        textcoords="offset points",
        xytext=(9, 14),
        fontsize=9,
    )
    axes.annotate(
        f"{anti1EndVal:.2f}%",
        (endX, anti1EndVal),
        textcoords="offset points",
        xytext=(9, 14),
        fontsize=9,
    )
    axes.annotate(
        f"{anti2RestingVal:.2f}%",
        (0, anti2RestingVal),
        textcoords="offset points",
        xytext=(9, 14),
        fontsize=9,
    )
    axes.annotate(
        f"{anti2StartVal:.2f}%",
        (startX, anti2StartVal),
        textcoords="offset points",
        xytext=(9, 14),
        fontsize=9,
    )
    axes.annotate(
        f"{anti2EndVal:.2f}%",
        (endX, anti2EndVal),
        textcoords="offset points",
        xytext=(9, 14),
        fontsize=9,
    )

    axes.set_title(plot_title)
    axes.set_xlabel("Axle Travel (inches)")
    axes.set_ylabel("Anti Value (%)")
    axes.grid(True)
    axes.legend()

    axes.axhspan(50, 65, alpha=0.1)


def plot_pinion_angle_change(axes, title, travel, pinion_angle_chng):
    axes.plot(travel, pinion_angle_chng, label="Pinion Angle Change", color="tab:blue")

    restingIndex = travel.index(0)
    startX = travel[0]
    endX = travel[-1]
    pinion_resting_val = pinion_angle_chng[restingIndex]
    pinion_start_val = pinion_angle_chng[0]
    pinion_end_val = pinion_angle_chng[-1]

    axes.plot(0, pinion_resting_val, "o", ms=4, color="tab:blue")
    axes.plot(startX, pinion_start_val, "o", ms=4, color="tab:blue")
    axes.plot(endX, pinion_end_val, "o", ms=4, color="tab:blue")

    axes.annotate(
        f"{pinion_resting_val:.2f} deg",
        (0, pinion_resting_val),
        textcoords="offset points",
        xytext=(9, 14),
        fontsize=9,
    )
    axes.annotate(
        f"{pinion_start_val:.2f} deg",
        (startX, pinion_start_val),
        textcoords="offset points",
        xytext=(9, 14),
        fontsize=9,
    )
    axes.annotate(
        f"{pinion_end_val:.2f} deg",
        (endX, pinion_end_val),
        textcoords="offset points",
        xytext=(9, 14),
        fontsize=9,
    )

    axes.set_title(title)
    axes.legend()
    axes.grid(True)

    axes.set_xlabel("Axle Travel (inches)")
    axes.set_ylabel("Pinion Angle Change from Resting (deg)")


def plot_all(data, out_path):
    fig, ((tl, tr), (bl, br)) = plt.subplots(2, 2, figsize=(14, 9))

    (
        front_travel,
        rear_travel,
        front_pinion_angle,
        rear_pinion_angle,
        anti_squat,
        anti_rise,
        anti_lift,
        anti_dive,
    ) = list_data(data)

    plot_anti(tl, "Front", front_travel, "Anti-lift", anti_lift, "Anti-dive", anti_dive)
    plot_anti(tr, "Rear", rear_travel, "Anti-squat", anti_squat, "Anti-rise", anti_rise)
    plot_pinion_angle_change(bl, "Front", front_travel, front_pinion_angle)
    plot_pinion_angle_change(br, "Rear", rear_travel, rear_pinion_angle)

    fig.savefig(out_path, dpi=250, bbox_inches="tight", pad_inches=0.25)


def main():
    if args_valid(sys.argv):
        in_path = sys.argv[1]
        out_path = sys.argv[2]

        data = get_data(in_path)
        plot_all(data, out_path)
    else:
        print("Usage: python plot.py <data.csv> <output_path.png>")


if __name__ == "__main__":
    main()
