# You are given an integer array nums and two integers k and m.

# Return an integer denoting the count of of nums such that:

#     The subarray contains exactly k distinct integers.
#     Within the subarray, each distinct integer appears at least m times.

 

# Example 1:

# Input: nums = [1,2,1,2,2], k = 2, m = 2

# Output: 2

# Explanation:

# The possible subarrays with k = 2 distinct integers, each appearing at least m = 2 times are:
# Subarray	Distinct
# numbers	Frequency
# [1, 2, 1, 2]	{1, 2} → 2	{1: 2, 2: 2}
# [1, 2, 1, 2, 2]	{1, 2} → 2	{1: 2, 2: 3}

# Thus, the answer is 2.

# Example 2:

# Input: nums = [3,1,2,4], k = 2, m = 1

# Output: 3

# Explanation:

# The possible subarrays with k = 2 distinct integers, each appearing at least m = 1 times are:
# Subarray	Distinct
# numbers	Frequency
# [3, 1]	{3, 1} → 2	{3: 1, 1: 1}
# [1, 2]	{1, 2} → 2	{1: 1, 2: 1}
# [2, 4]	{2, 4} → 2	{2: 1, 4: 1}

# Thus, the answer is 3.

 

# Constraints:

#     1 <= nums.length <= 10^5
#     1 <= nums[i] <= 10^5
#     1 <= k, m <= nums.length

 
# Seen this question in a real interview before?
# 1/5
# Yes
# No
# Accepted
# 3,172/22.9K
# Acceptance Rate
# 13.9%
# Topics
# Weekly Contest 491
# icon
# Companies
# Hint 1
# Use sliding window.
# Hint 2
# Use the reduction: answer = at_most(k) - at_most(k-1). at_most(K) = number of subarrays with <= K distinct values where every present value appears >= m times.


class Solution:
    def countSubarrays(self, nums: list[int], k: int, m: int) -> int:
        if k <= 0:
            return 0

        n = len(nums)
        fenwick = FenwickTree(n)
        seg_tree = ZeroCountSegmentTree(n)

        positions: dict[int, list[int]] = {}

        total = 0
        fenwick_add = fenwick.add
        find_kth = fenwick.find_kth
        range_add = seg_tree.range_add
        count_zeros = seg_tree.count_zeros
        k_minus_one = k - 1

        for right, value in enumerate(nums):
            value_positions = positions.get(value)
            if value_positions is None:
                value_positions = []
                positions[value] = value_positions

            if value_positions:
                prev_last = value_positions[-1]
                prev_mth_latest = value_positions[-m] if len(value_positions) >= m else -1
                range_add(prev_mth_latest + 1, prev_last, -1)
                fenwick_add(prev_last, -1)

            value_positions.append(right)
            curr_last = right
            curr_mth_latest = value_positions[-m] if len(value_positions) >= m else -1
            range_add(curr_mth_latest + 1, curr_last, 1)
            fenwick_add(curr_last, 1)

            distinct_count = len(positions)
            rank_k = distinct_count - k
            rank_k_minus_one = distinct_count - k_minus_one
            left_bound_k = 0 if rank_k <= 0 else find_kth(rank_k) + 1
            left_bound_k_minus_one = (
                0 if rank_k_minus_one <= 0 else find_kth(rank_k_minus_one) + 1
            )

            total += count_zeros(left_bound_k, left_bound_k_minus_one - 1)

        return total


class FenwickTree:
    def __init__(self, size: int):
        self.size = size
        self.data = [0] * (size + 1)

    def add(self, index: int, delta: int) -> None:
        i = index + 1
        while i <= self.size:
            self.data[i] += delta
            i += i & -i

    def find_kth(self, k: int) -> int:
        index = 0
        bit = 1
        while bit * 2 <= self.size:
            bit *= 2

        curr = 0
        while bit > 0:
            next_index = index + bit
            if next_index <= self.size and curr + self.data[next_index] < k:
                curr += self.data[next_index]
                index = next_index
            bit //= 2
        return index


class ZeroCountSegmentTree:
    def __init__(self, size: int):
        self.size = size
        self.min_value = [0] * (4 * size)
        self.min_count = [0] * (4 * size)
        self.lazy = [0] * (4 * size)
        self._build(1, 0, size - 1)

    def _build(self, node: int, left: int, right: int) -> None:
        if left == right:
            self.min_count[node] = 1
            return

        mid = (left + right) // 2
        self._build(node * 2, left, mid)
        self._build(node * 2 + 1, mid + 1, right)
        self._pull(node)

    def _apply(self, node: int, delta: int) -> None:
        self.min_value[node] += delta
        self.lazy[node] += delta

    def _push(self, node: int) -> None:
        if self.lazy[node] != 0:
            delta = self.lazy[node]
            self._apply(node * 2, delta)
            self._apply(node * 2 + 1, delta)
            self.lazy[node] = 0

    def _pull(self, node: int) -> None:
        left_child = node * 2
        right_child = node * 2 + 1
        left_min = self.min_value[left_child]
        right_min = self.min_value[right_child]

        self.min_value[node] = min(left_min, right_min)
        self.min_count[node] = 0
        if left_min == self.min_value[node]:
            self.min_count[node] += self.min_count[left_child]
        if right_min == self.min_value[node]:
            self.min_count[node] += self.min_count[right_child]

    def range_add(self, q_left: int, q_right: int, delta: int) -> None:
        if q_left > q_right:
            return
        self._range_add(1, 0, self.size - 1, q_left, q_right, delta)

    def _range_add(
        self,
        node: int,
        left: int,
        right: int,
        q_left: int,
        q_right: int,
        delta: int,
    ) -> None:
        if q_left <= left and right <= q_right:
            self._apply(node, delta)
            return

        self._push(node)
        mid = (left + right) // 2
        if q_left <= mid:
            self._range_add(node * 2, left, mid, q_left, q_right, delta)
        if q_right > mid:
            self._range_add(node * 2 + 1, mid + 1, right, q_left, q_right, delta)
        self._pull(node)

    def count_zeros(self, q_left: int, q_right: int) -> int:
        if q_left > q_right:
            return 0
        return self._count_zeros(1, 0, self.size - 1, q_left, q_right)

    def _count_zeros(
        self,
        node: int,
        left: int,
        right: int,
        q_left: int,
        q_right: int,
    ) -> int:
        if q_left <= left and right <= q_right:
            return self.min_count[node] if self.min_value[node] == 0 else 0

        self._push(node)
        mid = (left + right) // 2
        result = 0
        if q_left <= mid:
            result += self._count_zeros(node * 2, left, mid, q_left, q_right)
        if q_right > mid:
            result += self._count_zeros(node * 2 + 1, mid + 1, right, q_left, q_right)
        return result
