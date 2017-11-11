#include <limits.h>
#include <sys/ioctl.h>
#include <unistd.h>

/**
 * @brief Get the height of the terminal in term of visible lines
 * @return the height of the terminal or UINT_MAX in case of error
 */
unsigned int getTerminalHeight()
{
    struct winsize w;
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &w) < 0) {
        return UINT_MAX;
    }

    return w.ws_row;
}
