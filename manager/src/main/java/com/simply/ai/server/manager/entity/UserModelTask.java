package com.simply.ai.server.manager.entity;

import com.baomidou.mybatisplus.annotation.*;
import com.baomidou.mybatisplus.extension.handlers.JacksonTypeHandler;
import lombok.Builder;
import lombok.Data;
import lombok.experimental.Accessors;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

@Data
@Builder
@TableName(value = "user_model_task", autoResultMap = true)
@Accessors(chain = true)
public class UserModelTask {

    @TableId(type = IdType.AUTO)
    private Integer id;

    private Integer userId;

    private String recordId;

    private Integer modelId;

    private Integer pricingId;

    @Builder.Default
    private Integer status = 0;

    private String failedReason;

    private String thirdTaskId;

    // JSON数组字段
    @TableField(value = "input_urls", typeHandler = JacksonTypeHandler.class)
    private List<String> inputUrls;

    @TableField(value = "output_urls", typeHandler = JacksonTypeHandler.class)
    private List<String> outputUrls;

    @TableField(value = "input_details", typeHandler = JacksonTypeHandler.class)
    private Object inputDetails;

    @TableField(value = "output_details", typeHandler = JacksonTypeHandler.class)
    private Object outputDetails;

    @TableField(value = "output_callback_details", typeHandler = JacksonTypeHandler.class)
    private Object outputCallbackDetails;

    @Builder.Default
    private Integer isDel = 0;

    @TableField(fill = FieldFill.INSERT)
    private LocalDateTime gmtCreate;

    @TableField(fill = FieldFill.INSERT_UPDATE)
    private LocalDateTime gmtModified;

    /**
     * 创建包含全量参数的任务对象
     */
    public static UserModelTask create(Integer userId, String recordId, Integer modelId,
                                       Integer pricingId, Integer status, String failedReason,
                                       String thirdTaskId, List<String> inputUrls, List<String> outputUrls,
                                       Object inputDetails, Object outputDetails,
                                       Object outputCallbackDetails) {
        return UserModelTask.builder()
                .userId(userId)
                .recordId(recordId)
                .modelId(modelId)
                .pricingId(pricingId)
                .status(status)
                .failedReason(failedReason)
                .thirdTaskId(thirdTaskId)
                .inputUrls(inputUrls)
                .outputUrls(outputUrls)
                .inputDetails(inputDetails)
                .outputDetails(outputDetails)
                .outputCallbackDetails(outputCallbackDetails)
                .build();
    }
}