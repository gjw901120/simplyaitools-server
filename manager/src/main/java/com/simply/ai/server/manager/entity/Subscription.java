package com.simply.ai.server.manager.entity;

import com.baomidou.mybatisplus.annotation.*;
import com.simply.ai.server.manager.enums.SubscriptionPackageEnum;
import com.simply.ai.server.manager.enums.SubscriptionTypeEnum;
import lombok.Data;

import java.time.LocalDate;
import java.time.LocalDateTime;

@Data
@TableName("subscription")
public class Subscription {

    @TableId(type = IdType.AUTO)
    private Integer id;

    private Integer orderId;

    private Integer userId;

    private String stripeSubscriptionId;

    private SubscriptionTypeEnum type;

    private Integer status;

    @TableField("`package`")
    private SubscriptionPackageEnum SubscriptionPackage;

    private LocalDate startDate;

    private LocalDate endDate;

    private Integer isDel;

    @TableField(fill = FieldFill.INSERT)
    private LocalDateTime gmtCreate;

    @TableField(fill = FieldFill.INSERT_UPDATE)
    private LocalDateTime gmtModified;
}