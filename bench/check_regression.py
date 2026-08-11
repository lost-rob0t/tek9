#!/usr/bin/env python3
"""Fail CI when a Tek9 performance regression exceeds both signal and noise."""

from __future__ import annotations

import argparse
import json
import math
import statistics
import sys
from pathlib import Path


def load(path: Path) -> dict:
    with path.open("r", encoding="utf-8") as handle:
        return json.load(handle)


def fast_mad(metric: dict) -> float:
    """Return the run-to-run fast-path MAD, including schema-v2 fallback."""
    if "fast_mad_seconds" in metric:
        return float(metric["fast_mad_seconds"])

    samples = [float(value) for value in metric.get("fast_run_seconds", [])]
    if not samples:
        return 0.0
    center = statistics.median(samples)
    return statistics.median(abs(value - center) for value in samples)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("baseline", type=Path)
    parser.add_argument("current", type=Path)
    parser.add_argument(
        "--max-regression-pct",
        type=float,
        default=5.0,
        help="Allowed median fast-path slowdown before CI considers regression.",
    )
    parser.add_argument(
        "--noise-mad-multiplier",
        type=float,
        default=3.0,
        help=(
            "Require the absolute slowdown to exceed this multiple of the larger "
            "baseline/current median absolute deviation as well as the percentage gate."
        ),
    )
    args = parser.parse_args()

    if args.max_regression_pct < 0:
        parser.error("--max-regression-pct must be non-negative")
    if args.noise_mad_multiplier < 0:
        parser.error("--noise-mad-multiplier must be non-negative")

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
        f"(median slowdown > {args.max_regression_pct:.2f}% and "
        f"> {args.noise_mad_multiplier:.2f}x observed MAD required to fail)"
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

        old_mad = fast_mad(old_metric)
        new_mad = fast_mad(new_metric)
        if not all(math.isfinite(value) and value >= 0 for value in (old_mad, new_mad)):
            failures.append(f"{name}: non-finite/negative fast-path MAD")
            continue

        slowdown = new_fast - old_fast
        relative_limit = old_fast * tolerance
        noise_limit = args.noise_mad_multiplier * max(old_mad, new_mad)
        fast_change = (new_fast / old_fast - 1.0) * 100.0
        speedup_change = (
            (new_speedup / old_speedup - 1.0) * 100.0
            if old_speedup > 0 and new_speedup > 0
            else float("nan")
        )

        relative_regression = slowdown > relative_limit
        noise_significant = slowdown > noise_limit
        regressed = relative_regression and noise_significant
        status = "REGRESSION" if regressed else ("NOISY PASS" if relative_regression else "PASS")

        print(
            f"{status:10s} {name:40s} "
            f"median-fast {old_fast:.9f}s -> {new_fast:.9f}s {fast_change:+8.2f}% | "
            f"MAD {old_mad:.9f}s/{new_mad:.9f}s | "
            f"score {old_speedup:.4f}x -> {new_speedup:.4f}x {speedup_change:+8.2f}%"
        )

        if regressed:
            failures.append(
                f"{name}: median fast path {old_fast:.9f}s -> {new_fast:.9f}s "
                f"({fast_change:+.2f}%), slowdown {slowdown:.9f}s exceeds "
                f"relative limit {relative_limit:.9f}s and noise limit {noise_limit:.9f}s"
            )

    extra = sorted(set(current_metrics) - set(baseline_metrics))
    for name in extra:
        print(f"NEW        {name:40s} no parent metric; recording without gating")

    if failures:
        print("\nPerformance regression detected:")
        for failure in failures:
            print(f"  - {failure}")
        return 1

    print("\nNo statistically meaningful median fast-path regression detected.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
