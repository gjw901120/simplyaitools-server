package com.simply.ai.server.manager.model.request;

import com.simply.ai.server.manager.enums.SunoModelEnum;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Positive;
import java.io.Serializable;

/**
 * 延长音乐请求参数
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class SunoExtendRequest extends SunoBaseRequest implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 音频ID
     */
    @NotBlank(message = "音频ID不能为空")
    private String audioId;

    /**
     * 是否使用自定义参数
     */
    @NotNull(message = "参数标志不能为空")
    private Boolean defaultParamFlag;

    /**
     * 扩展提示词
     */
    private String prompt;

    /**
     * 音乐风格
     */
    private String style;

    /**
     * 音乐标题
     */
    private String title;

    /**
     * 扩展起始时间点
     */
    @Positive(message = "扩展起始时间必须大于0")
    private Double continueAt;

    /**
     * 业务参数校验
     */
    public void validateBusinessRules() {
        validateWeightParameters();

        if (Boolean.TRUE.equals(defaultParamFlag)) {
            if (prompt == null || prompt.trim().isEmpty()) {
                throw new IllegalArgumentException("自定义参数模式下提示词不能为空");
            }
            if (style == null || style.trim().isEmpty()) {
                throw new IllegalArgumentException("自定义参数模式下风格不能为空");
            }
            if (title == null || title.trim().isEmpty()) {
                throw new IllegalArgumentException("自定义参数模式下标题不能为空");
            }
            if (continueAt == null || continueAt <= 0) {
                throw new IllegalArgumentException("自定义参数模式下扩展起始时间必须大于0");
            }

            // 模型特定的长度校验 - 使用修正后的方法
            int promptMaxLength = getPromptMaxLength();
            if (prompt.length() > promptMaxLength) {
                throw new IllegalArgumentException("提示词长度不能超过" + promptMaxLength + "个字符");
            }

            int styleMaxLength = getStyleMaxLength();
            if (style.length() > styleMaxLength) {
                throw new IllegalArgumentException("风格描述长度不能超过" + styleMaxLength + "个字符");
            }

            int titleMaxLength = getTitleMaxLength();
            if (title.length() > titleMaxLength) {
                throw new IllegalArgumentException("标题长度不能超过" + titleMaxLength + "个字符");
            }
        }
    }

    /**
     * 构建自定义参数延长请求
     */
    public static SunoExtendRequest customParams(String audioId, String prompt, String style, String title,
                                                 Double continueAt, SunoModelEnum model, String callBackUrl) {
        SunoExtendRequest request = new SunoExtendRequest();
        request.setAudioId(audioId);
        request.setDefaultParamFlag(true);
        request.setPrompt(prompt);
        request.setStyle(style);
        request.setTitle(title);
        request.setContinueAt(continueAt);
        request.setModel(model);
        request.setCallBackUrl(callBackUrl);
        return request;
    }

    /**
     * 构建继承参数延长请求
     */
    public static SunoExtendRequest inheritParams(String audioId, SunoModelEnum model, String callBackUrl) {
        SunoExtendRequest request = new SunoExtendRequest();
        request.setAudioId(audioId);
        request.setDefaultParamFlag(false);
        request.setModel(model);
        request.setCallBackUrl(callBackUrl);
        return request;
    }
}