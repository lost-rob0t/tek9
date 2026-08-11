#!/usr/bin/env python3
"""Aggregate repeated Tek9 benchmark JSON files using medians."""

from __future__ import annotations

import argparse
import json
import statistics
from pathlib import Path


NUMERIC_FIELDS = ("reference_seconds", "fast_seconds", "speedup")


def load(path: Path) -> dict:
    with path.open("r", encoding="utf-8") as handle:
        return json.load(handle)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("inputs", nargs="+", type=Path)
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("--commit", required=True)
    parser.add_argument("--baseline-commit")
    args = parser.parse_args()

    runs = [load(path) for path in args.inputs]
    if not runs:
        raise SystemExit("no benchmark runs supplied")

    metric_names = set(runs[0].get("metrics", {}))
    for run in runs[1:]:
        if set(run.get("metrics", {})) != metric_names:
            raise SystemExit("benchmark runs contain different metric sets")

    metrics: dict[str, dict] = {}
    for name in sorted(metric_names):
        samples = [run["metrics"][name] for run in runs]
        aggregate = {
            field: statistics.median(float(sample[field]) for sample in samples)
            for field in NUMERIC_FIELDS
        }
        aggregate["reference_samples"] = int(samples[0].get("reference_samples", 1))
        aggregate["fast_samples"] = int(samples[0].get("fast_samples", 1))
        aggregate["runs"] = len(samples)
        metrics[name] = aggregate

    result = {
        "schema_version": 2,
        "commit": args.commit,
        "durability": runs[0].get("durability", "unknown"),
        "aggregation": "median",
        "run_count": len(runs),
        "metrics": metrics,
    }
    if args.baseline_commit:
        result["baseline_commit"] = args.baseline_commit

    args.output.parent.mkdir(parents=True, exist_ok=True)
    with args.output.open("w", encoding="utf-8") as handle:
        json.dump(result, handle, indent=2, sort_keys=True)
        handle.write("\n")

    print(f"Wrote median of {len(runs)} runs to {args.output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
