package com.simply.ai.server.manager.entity;

import com.baomidou.mybatisplus.annotation.*;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

@Data
@TableName("subscription_plan")
public class SubscriptionPlan {

    @TableId(type = IdType.AUTO)
    private Integer id;

    private Integer userId;

    private Integer subscriptionId;

    private LocalDate startDate;

    private LocalDate endDate;

    private BigDecimal amount;

    private BigDecimal giftAmount;

    private Integer status;

    private Integer isDel;

    @TableField(fill = FieldFill.INSERT)
    private LocalDateTime gmtCreate;

    @TableField(fill = FieldFill.INSERT_UPDATE)
    private LocalDateTime gmtModified;
}