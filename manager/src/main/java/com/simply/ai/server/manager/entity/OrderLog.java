package com.simply.ai.server.manager.entity;

import com.baomidou.mybatisplus.annotation.*;
import lombok.Data;

import java.time.LocalDateTime;

@Data
@TableName("order_log")
public class OrderLog {

    @TableId(type = IdType.AUTO)
    private Integer id;

    private Integer orderId;

    private String detail;

    @TableField(fill = FieldFill.INSERT)
    private LocalDateTime gmtCreate;

    @TableField(fill = FieldFill.INSERT_UPDATE)
    private LocalDateTime gmtModified;
}