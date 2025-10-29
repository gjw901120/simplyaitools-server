// SunoGenerateRequest.java
package com.simply.ai.server.manager.model.request;

import com.simply.ai.server.manager.constant.SunoMusicConstant;
import com.simply.ai.server.manager.enums.SunoModelEnum;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.io.Serializable;

/**
 * 生成音乐请求参数
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class SunoGenerateRequest extends SunoBaseRequest implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 描述所需音频内容的提示词
     */
    @NotBlank(message = "提示词不能为空")
    private String prompt;

    /**
     * 是否启用自定义模式
     */
    @NotNull(message = "自定义模式不能为空")
    private Boolean customMode;

    /**
     * 是否为纯音乐
     */
    @NotNull(message = "是否为纯音乐不能为空")
    private Boolean instrumental;

    /**
     * 音乐风格
     */
    @Size(max = 1000, message = "风格描述长度不能超过1000个字符")
    private String style;

    /**
     * 音乐标题
     */
    @Size(max = 100, message = "标题长度不能超过100个字符")
    private String title;

    /**
     * 业务参数校验
     */
    public void validateBusinessRules() {
        validateWeightParameters();

        // 自定义模式下的校验
        if (Boolean.TRUE.equals(customMode)) {
            if (style == null || style.trim().isEmpty()) {
                throw new IllegalArgumentException("自定义模式下风格不能为空");
            }
            if (title == null || title.trim().isEmpty()) {
                throw new IllegalArgumentException("自定义模式下标题不能为空");
            }
            if (Boolean.FALSE.equals(instrumental) && (prompt == null || prompt.trim().isEmpty())) {
                throw new IllegalArgumentException("自定义非纯音乐模式下提示词不能为空");
            }

            // 模型特定的长度校验 - 使用修正后的方法
            int styleMaxLength = getStyleMaxLength();
            if (style.length() > styleMaxLength) {
                throw new IllegalArgumentException("风格描述长度不能超过" + styleMaxLength + "个字符");
            }

            int titleMaxLength = getTitleMaxLength();
            if (title.length() > titleMaxLength) {
                throw new IllegalArgumentException("标题长度不能超过" + titleMaxLength + "个字符");
            }

            if (Boolean.FALSE.equals(instrumental)) {
                int promptMaxLength = getPromptMaxLength();
                if (prompt.length() > promptMaxLength) {
                    throw new IllegalArgumentException("提示词长度不能超过" + promptMaxLength + "个字符");
                }
            }
        } else {
            // 非自定义模式下的校验
            if (prompt.length() > SunoMusicConstant.PROMPT_MAX_LENGTH_NON_CUSTOM) {
                throw new IllegalArgumentException("非自定义模式下提示词长度不能超过500个字符");
            }
        }
    }

    /**
     * 构建自定义模式请求
     */
    public static SunoGenerateRequest customMode(String prompt, String style, String title,
                                                 Boolean instrumental, SunoModelEnum model, String callBackUrl) {
        SunoGenerateRequest request = new SunoGenerateRequest();
        request.setPrompt(prompt);
        request.setStyle(style);
        request.setTitle(title);
        request.setInstrumental(instrumental);
        request.setCustomMode(true);
        request.setModel(model);
        request.setCallBackUrl(callBackUrl);
        return request;
    }

    /**
     * 构建非自定义模式请求
     */
    public static SunoGenerateRequest simpleMode(String prompt, SunoModelEnum model, String callBackUrl) {
        SunoGenerateRequest request = new SunoGenerateRequest();
        request.setPrompt(prompt);
        request.setCustomMode(false);
        request.setModel(model);
        request.setCallBackUrl(callBackUrl);
        request.setInstrumental(true); // 非自定义模式默认为纯音乐
        return request;
    }
}