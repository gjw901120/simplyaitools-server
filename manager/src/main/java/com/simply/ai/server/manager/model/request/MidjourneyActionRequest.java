package com.simply.ai.server.manager.model.request;

import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.NotBlank;

/**
 * Action 动作请求参数
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class MidjourneyActionRequest extends MidjourneyBaseRequest {

    private static final long serialVersionUID = 1L;

    /**
     * 是否选择同一频道下的账号
     */
    private Boolean chooseSameChannel = false;

    /**
     * 动作标识符
     */
    @NotBlank(message = "动作标识符不能为空")
    private String customId;

    /**
     * 要执行动作的任务 ID
     */
    @NotBlank(message = "任务ID不能为空")
    private String taskId;

    /**
     * 构建 Action 请求
     */
    public static MidjourneyActionRequest build(String taskId, String customId) {
        MidjourneyActionRequest request = new MidjourneyActionRequest();
        request.setTaskId(taskId);
        request.setCustomId(customId);
        return request;
    }

    /**
     * 流式构建方法
     */
    public static MidjourneyActionRequest create(String taskId, String customId) {
        return new MidjourneyActionRequest().withTaskId(taskId).withCustomId(customId);
    }

    /**
     * 设置任务ID
     */
    public MidjourneyActionRequest withTaskId(String taskId) {
        this.taskId = taskId;
        return this;
    }

    /**
     * 设置动作标识符
     */
    public MidjourneyActionRequest withCustomId(String customId) {
        this.customId = customId;
        return this;
    }

    /**
     * 设置选择同一频道
     */
    public MidjourneyActionRequest withChooseSameChannel(boolean chooseSameChannel) {
        this.chooseSameChannel = chooseSameChannel;
        return this;
    }
}