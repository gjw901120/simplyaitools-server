package com.simply.ai.server.manager.entity;

import com.baomidou.mybatisplus.annotation.*;
import com.simply.ai.server.manager.enums.OrderStatusEnum;
import com.simply.ai.server.manager.enums.OrderTypeEnum;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Data
@TableName("`order`")
public class Order {

    @TableId(type = IdType.AUTO)
    private Integer id;

    private String uuid;

    private Integer userId;

    private OrderTypeEnum type;

    private Integer couponId;

    private BigDecimal paymentAmount;

    private BigDecimal discountAmount;

    private BigDecimal amount;

    private BigDecimal giftAmount;

    private OrderStatusEnum status;

    private String stripePaymentIntentId;

    private String stripeCustomerId;

    private String stripeInvoiceId;

    private Integer isDel;

    @TableField(fill = FieldFill.INSERT)
    private LocalDateTime gmtCreate;

    @TableField(fill = FieldFill.INSERT_UPDATE)
    private LocalDateTime gmtModified;
}