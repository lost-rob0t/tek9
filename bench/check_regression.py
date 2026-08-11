#!/usr/bin/env python3
"""Fail CI when a Tek9 benchmark score regresses from the committed baseline."""

from __future__ import annotations

import argparse
import json
import math
import sys
from pathlib import Path


def load(path: Path) -> dict:
    with path.open("r", encoding="utf-8") as handle:
        return json.load(handle)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("baseline", type=Path)
    parser.add_argument("current", type=Path)
    parser.add_argument(
        "--max-regression-pct",
        type=float,
        default=5.0,
        help="Allowed speedup-score drop for CI noise before failing.",
    )
    args = parser.parse_args()

    baseline = load(args.baseline)
    current = load(args.current)
    baseline_metrics = baseline.get("metrics", {})
    current_metrics = current.get("metrics", {})

    missing = sorted(set(baseline_metrics) - set(current_metrics))
    if missing:
        print(f"ERROR: current benchmark is missing metrics: {', '.join(missing)}")
        return 2

    failures: list[str] = []
    tolerance = args.max_regression_pct / 100.0

    print(
        f"Comparing {current.get('commit', 'unknown')} against "
        f"baseline {baseline.get('commit', 'unknown')} "
        f"(max regression {args.max_regression_pct:.2f}%)"
    )

    for name in sorted(baseline_metrics):
        old = float(baseline_metrics[name]["speedup"])
        new = float(current_metrics[name]["speedup"])
        if not (math.isfinite(old) and math.isfinite(new) and old > 0 and new > 0):
            failures.append(f"{name}: non-finite/non-positive score old={old} new={new}")
            continue

        change = (new / old - 1.0) * 100.0
        status = "PASS"
        if new < old * (1.0 - tolerance):
            status = "REGRESSION"
            failures.append(
                f"{name}: {old:.4f}x -> {new:.4f}x ({change:.2f}%)"
            )
        print(f"{status:10s} {name:42s} {old:10.4f}x -> {new:10.4f}x {change:+8.2f}%")

    if failures:
        print("\nPerformance regression detected:")
        for failure in failures:
            print(f"  - {failure}")
        return 1

    print("\nNo benchmark regression exceeded the configured threshold.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
