/* Function below main (_start) is part of this object.
 * So we use main() in this object as a marker for
 * functions in inlinfo_nested.so.
 */

extern int main_nested(void);

int main() {
   return main_nested();
}

