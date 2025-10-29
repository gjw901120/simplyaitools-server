package com.simply.ai.server.manager.entity;

import com.baomidou.mybatisplus.annotation.*;
import com.simply.ai.server.manager.enums.UserCreditTypeEnum;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Data
@TableName("bill")
public class Bill {

    @TableId(type = IdType.AUTO)
    private Integer id;

    private Integer userId;

    private Integer modelId;

    private Integer pricingId;

    private Integer messageId;

    private UserCreditTypeEnum userCreditType;

    private BigDecimal actualDeductCredits;

    private BigDecimal deductCredits;

    private BigDecimal originCredits;

    private Integer discount;

    private Integer isFree;

    private Integer isDel;

    @TableField(fill = FieldFill.INSERT)
    private LocalDateTime gmtCreate;

    @TableField(fill = FieldFill.INSERT_UPDATE)
    private LocalDateTime gmtModified;
}