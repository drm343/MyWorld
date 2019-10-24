#include "BaseClass.h"


/* @brief 建立新的 class id，必須手動 free
 * @return 新建立的 Class
 */
Class NEW_CLASS_ID(void)
{
    Class class = NEW(Class);

    uuid_t binary_uuid;
    uuid_generate(binary_uuid);
    class->id = malloc(37);
    uuid_unparse(binary_uuid, class->id);

    class->counter = 1;
    return class;
}


/* @brief 釋放不需要的 class
 * @param class 要釋放的 Class
 */
void RELEASE_CLASS(Class class)
{
    if (class != NULL) {
        class->counter = class->counter - 1;
        if (class->counter <= 0) {
            free(class->id);
            free(class);
            class = NULL;
        }
    }
}


/* @brief 檢查兩個 Class 是否相同
 * @param class_1
 * @param class_2
 * @return 檢查結果
 */
bool CHECK_CLASS(Class class_1, Class class_2)
{
    if (strcmp(class_1->id, class_2->id) == 0) {
        return true;
    }
    return false;
}
