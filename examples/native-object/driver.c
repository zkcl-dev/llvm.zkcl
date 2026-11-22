#include <inttypes.h>
#include <stdio.h>

struct Order {
  uint32_t quantity;
  uint64_t unit_price_cents;
  uint32_t shipping_cents;
};

extern uint64_t order_total(const struct Order *order);

int main(void) {
  const struct Order order = {3, 275, 20};
  uint64_t total = order_total(&order);
  printf("Order total = %" PRIu64 " cents\n", total);
  return total == 845 ? 0 : 1;
}
