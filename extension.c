#include <sys/ioctl.h>

int termsize()
{
  struct winsize w;
  ioctl(0,TIOCGWINSZ, &w);
  return(w.ws_row);
}
