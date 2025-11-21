package com.simply.ai.server.manager.entity;

import com.baomidou.mybatisplus.annotation.*;
import com.simply.ai.server.manager.enums.SubscriptionPackageEnum;
import lombok.Builder;
import lombok.Data;
import lombok.experimental.Accessors;

import java.time.LocalDateTime;

@Data
@Builder
@TableName("user")
@Accessors(chain = true)
public class User {

    @TableId(type = IdType.AUTO)
    private Integer id;

    private String name;

    private String email;

    @Builder.Default
    private Integer isTopUp = 0;

    @Builder.Default
    private Integer isSubscription = 0;

    private SubscriptionPackageEnum subscriptionPackage;

    private String timeZone;

    @Builder.Default
    private Integer isDel = 0;

    @TableField(fill = FieldFill.INSERT)
    private LocalDateTime gmtCreate;

    @TableField(fill = FieldFill.INSERT_UPDATE)
    private LocalDateTime gmtModified;

    /**
     * 创建用户对象的便捷方法
     */
    public static User create(String name, String email, Integer isTopUp, Integer isSubscription, SubscriptionPackageEnum subscriptionPackage, String timeZone) {
        return User.builder()
                .name(name)
                .email(email)
                .isTopUp(isTopUp != null ? isTopUp : 0)
                .isSubscription(isSubscription != null ? isSubscription : 0)
                .subscriptionPackage(subscriptionPackage)
                .timeZone(timeZone)
                .build();
    }
}