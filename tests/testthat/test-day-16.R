test_that(test_name(day = 16, part = 1), {
  expect_true(sum_versions(parse_packet(decode_bits("8A004A801A8002F478"))) == 16)
  expect_true(sum_versions(parse_packet(decode_bits("620080001611562C8802118E34"))) == 12)
  expect_true(sum_versions(parse_packet(decode_bits("C0015000016115A2E0802F182340"))) == 23)
  expect_true(sum_versions(parse_packet(decode_bits("A0016C880162017C3686B18A3D4780"))) == 31)
})

test_that(test_name(day = 16, part = 2), {
  expect_true(eval_expression(parse_packet(decode_bits("C200B40A82"))) == 3)
  expect_true(eval_expression(parse_packet(decode_bits("04005AC33890"))) == 54)
  expect_true(eval_expression(parse_packet(decode_bits("880086C3E88112"))) == 7)
  expect_true(eval_expression(parse_packet(decode_bits("CE00C43D881120"))) == 9)
  expect_true(eval_expression(parse_packet(decode_bits("D8005AC2A8F0"))) == 1)
  expect_true(eval_expression(parse_packet(decode_bits("F600BC2D8F"))) == 0)
  expect_true(eval_expression(parse_packet(decode_bits("9C005AC2F8F0"))) == 0)
  expect_true(eval_expression(parse_packet(decode_bits("9C0141080250320F1802104A08"))) == 1)
})
