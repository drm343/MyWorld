#include "Morph.h"


typedef struct Morph {
    Point position;
    /**< position 起始座標 */
    Point extent;
    /**< extent 長度及寬度 */
    Color color;
    /**< color 顏色 */
    String name;
    /**< name Morph 的名稱 */
    struct Morph *owner;
    /**< owner 此 Morph 上層 Morph, NULL 表示為最上層 */
    struct Morph *submorph;
    /**< submorph 此 Morph 的下層 Morph, NULL 表示為最下層 */
} *Morph;
