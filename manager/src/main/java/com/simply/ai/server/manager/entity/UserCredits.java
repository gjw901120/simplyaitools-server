package com.simply.ai.server.manager.entity;

import com.baomidou.mybatisplus.annotation.*;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Data
@TableName("user_credits")
public class UserCredits {

    @TableId(type = IdType.AUTO)
    private Integer id;

    private Integer userId;

    private BigDecimal credits;

    private BigDecimal blockCredits;

    private Integer type;

    private Integer status;

    private Integer isDel;

    @TableField(fill = FieldFill.INSERT)
    private LocalDateTime gmtCreate;

    @TableField(fill = FieldFill.INSERT_UPDATE)
    private LocalDateTime gmtModified;
}