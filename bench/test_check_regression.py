#!/usr/bin/env python3
"""Regression tests for the noise-aware Tek9 benchmark gate."""

from __future__ import annotations

import contextlib
import io
import json
import sys
import tempfile
import unittest
from pathlib import Path

import check_regression


def metric(fast: float, mad: float, speedup: float = 2.0) -> dict:
    return {
        "fast_seconds": fast,
        "fast_mad_seconds": mad,
        "speedup": speedup,
    }


class RegressionGateTests(unittest.TestCase):
    def run_gate(self, baseline_metrics: dict, current_metrics: dict) -> tuple[int, str]:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            baseline = root / "baseline.json"
            current = root / "current.json"
            baseline.write_text(
                json.dumps({"commit": "old", "metrics": baseline_metrics}),
                encoding="utf-8",
            )
            current.write_text(
                json.dumps({"commit": "new", "metrics": current_metrics}),
                encoding="utf-8",
            )

            old_argv = sys.argv
            output = io.StringIO()
            try:
                sys.argv = ["check_regression.py", str(baseline), str(current)]
                with contextlib.redirect_stdout(output):
                    result = check_regression.main()
            finally:
                sys.argv = old_argv
            return result, output.getvalue()

    def test_small_median_shift_inside_observed_noise_passes(self) -> None:
        result, output = self.run_gate(
            {"point-read": metric(0.0175, 0.0005)},
            {"point-read": metric(0.0185, 0.0005)},
        )
        self.assertEqual(0, result)
        self.assertIn("NOISY PASS", output)

    def test_serialization_fuzz_at_noise_boundary_passes(self) -> None:
        result, output = self.run_gate(
            {"write-batch": metric(0.028500000, 0.000499999)},
            {"write-batch": metric(0.029999999, 0.0)},
        )
        self.assertEqual(0, result)
        self.assertIn("NOISY PASS", output)

    def test_large_shift_outside_noise_fails(self) -> None:
        result, output = self.run_gate(
            {"point-read": metric(0.100, 0.001)},
            {"point-read": metric(0.110, 0.001)},
        )
        self.assertEqual(1, result)
        self.assertIn("REGRESSION", output)

    def test_relative_threshold_still_applies(self) -> None:
        result, output = self.run_gate(
            {"point-read": metric(0.100, 0.0001)},
            {"point-read": metric(0.104, 0.0001)},
        )
        self.assertEqual(0, result)
        self.assertIn("PASS", output)

    def test_missing_metric_is_configuration_error(self) -> None:
        result, output = self.run_gate(
            {"point-read": metric(0.100, 0.001)},
            {},
        )
        self.assertEqual(2, result)
        self.assertIn("missing metrics", output)


if __name__ == "__main__":
    unittest.main()
