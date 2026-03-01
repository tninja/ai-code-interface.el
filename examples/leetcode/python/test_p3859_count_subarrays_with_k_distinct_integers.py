import time
from unittest import TestCase

from p3859_count_subarrays_with_k_distinct_integers import Solution


class SolutionTestCase(TestCase):
    def setUp(self):
        self.solution = Solution()

    def testExample1(self):
        self.assertEqual(2, self.solution.countSubarrays([1, 2, 1, 2, 2], 2, 2))

    def testExample2(self):
        self.assertEqual(3, self.solution.countSubarrays([3, 1, 2, 4], 2, 1))

    def testNoValidSubarray(self):
        self.assertEqual(0, self.solution.countSubarrays([1, 1, 1], 2, 1))

    def testSmallBruteforceCrossCheck(self):
        nums = [1, 2, 1, 3, 2]
        self.assertEqual(
            self._bruteforce(nums, 2, 2),
            self.solution.countSubarrays(nums, 2, 2),
        )

    def testLargeCasePerformance(self):
        max_seconds = 10.5
        nums = [i % 50 for i in range(100000)]
        start = time.perf_counter()
        self.solution.countSubarrays(nums, 25, 20)
        elapsed = time.perf_counter() - start
        self.assertLess(elapsed, max_seconds)

    def _bruteforce(self, nums: list[int], k: int, m: int) -> int:
        ans = 0
        n = len(nums)
        for left in range(n):
            counts = {}
            for right in range(left, n):
                value = nums[right]
                counts[value] = counts.get(value, 0) + 1
                if len(counts) == k and all(freq >= m for freq in counts.values()):
                    ans += 1
        return ans
