package com.simply.ai.server.manager.entity;

import com.baomidou.mybatisplus.annotation.*;
import com.simply.ai.server.manager.enums.SubscriptionPackageEnum;
import lombok.Data;

import java.time.LocalDateTime;

@Data
@TableName("user")
public class User {

    @TableId(type = IdType.AUTO)
    private Integer id;

    private String name;

    private String email;

    private Integer isTopUp;

    private Integer isSubscription;

    private SubscriptionPackageEnum subscriptionPackage;

    private String timeZone;

    private Integer isDel;

    @TableField(fill = FieldFill.INSERT)
    private LocalDateTime gmtCreate;

    @TableField(fill = FieldFill.INSERT_UPDATE)
    private LocalDateTime gmtModified;
}