package com.simply.ai.server.manager.entity;

import com.baomidou.mybatisplus.annotation.*;
import lombok.Data;

import java.time.LocalDateTime;

@Data
@TableName("conversion_message")
public class ConversionMessage {

    @TableId(type = IdType.AUTO)
    private Integer id;

    private Integer userId;

    private Integer modelId;

    private Integer conversionId;

    private Integer role;

    private String message;

    private String files;

    private Integer promptTokens;

    private Integer completionTokens;

    private Integer isDel;

    @TableField(fill = FieldFill.INSERT)
    private LocalDateTime gmtCreate;

    @TableField(fill = FieldFill.INSERT_UPDATE)
    private LocalDateTime gmtModified;
}