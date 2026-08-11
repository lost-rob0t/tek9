#!/usr/bin/env python3
"""Fail CI when a Tek9 benchmark regresses from the committed baseline."""

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
        help="Allowed degradation before a metric is considered regressed.",
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
        old_metric = baseline_metrics[name]
        new_metric = current_metrics[name]
        old_speedup = float(old_metric["speedup"])
        new_speedup = float(new_metric["speedup"])
        old_fast = float(old_metric["fast_seconds"])
        new_fast = float(new_metric["fast_seconds"])

        values = (old_speedup, new_speedup, old_fast, new_fast)
        if not all(math.isfinite(value) and value > 0 for value in values):
            failures.append(f"{name}: non-finite/non-positive benchmark value")
            continue

        speedup_change = (new_speedup / old_speedup - 1.0) * 100.0
        fast_change = (new_fast / old_fast - 1.0) * 100.0

        slower = new_fast > old_fast * (1.0 + tolerance)
        weaker = new_speedup < old_speedup * (1.0 - tolerance)
        regressed = slower and weaker
        status = "REGRESSION" if regressed else "PASS"

        print(
            f"{status:10s} {name:40s} "
            f"fast {old_fast:.9f}s -> {new_fast:.9f}s {fast_change:+8.2f}% | "
            f"score {old_speedup:.4f}x -> {new_speedup:.4f}x {speedup_change:+8.2f}%"
        )

        if regressed:
            failures.append(
                f"{name}: fast path {old_fast:.9f}s -> {new_fast:.9f}s "
                f"({fast_change:+.2f}%), speedup {old_speedup:.4f}x -> "
                f"{new_speedup:.4f}x ({speedup_change:+.2f}%)"
            )

    if failures:
        print("\nPerformance regression detected:")
        for failure in failures:
            print(f"  - {failure}")
        return 1

    print("\nNo benchmark had both a slower fast path and a weaker relative score.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
