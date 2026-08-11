#!/usr/bin/env python3
"""Fail CI when the current Tek9 commit is slower than its parent."""

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
        help="Allowed median fast-path slowdown before CI fails.",
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
        f"Comparing current {current.get('commit', 'unknown')} against "
        f"parent {baseline.get('commit', 'unknown')} on the same runner "
        f"(max median slowdown {args.max_regression_pct:.2f}%)"
    )

    for name in sorted(baseline_metrics):
        old_metric = baseline_metrics[name]
        new_metric = current_metrics[name]
        old_fast = float(old_metric["fast_seconds"])
        new_fast = float(new_metric["fast_seconds"])
        old_speedup = float(old_metric.get("speedup", 0.0))
        new_speedup = float(new_metric.get("speedup", 0.0))

        if not all(math.isfinite(value) and value > 0 for value in (old_fast, new_fast)):
            failures.append(f"{name}: non-finite/non-positive fast-path time")
            continue

        fast_change = (new_fast / old_fast - 1.0) * 100.0
        speedup_change = (
            (new_speedup / old_speedup - 1.0) * 100.0
            if old_speedup > 0 and new_speedup > 0
            else float("nan")
        )
        regressed = new_fast > old_fast * (1.0 + tolerance)
        status = "REGRESSION" if regressed else "PASS"

        print(
            f"{status:10s} {name:40s} "
            f"median-fast {old_fast:.9f}s -> {new_fast:.9f}s {fast_change:+8.2f}% | "
            f"score {old_speedup:.4f}x -> {new_speedup:.4f}x {speedup_change:+8.2f}%"
        )

        if regressed:
            failures.append(
                f"{name}: median fast path {old_fast:.9f}s -> {new_fast:.9f}s "
                f"({fast_change:+.2f}%)"
            )

    extra = sorted(set(current_metrics) - set(baseline_metrics))
    for name in extra:
        print(f"NEW        {name:40s} no parent metric; recording without gating")

    if failures:
        print("\nPerformance regression detected:")
        for failure in failures:
            print(f"  - {failure}")
        return 1

    print("\nNo median fast path regressed beyond the configured threshold.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
