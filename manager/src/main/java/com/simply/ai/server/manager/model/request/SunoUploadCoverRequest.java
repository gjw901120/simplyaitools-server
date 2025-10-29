package com.simply.ai.server.manager.model.request;

import com.simply.ai.server.manager.constant.SunoMusicConstant;
import com.simply.ai.server.manager.enums.SunoModelEnum;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.hibernate.validator.constraints.URL;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.io.Serializable;

/**
 * 上传并翻唱请求参数
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class SunoUploadCoverRequest extends SunoBaseRequest implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 上传音频URL
     */
    @NotBlank(message = "上传URL不能为空")
    @URL(message = "上传URL格式不正确")
    private String uploadUrl;

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
     * 提示词
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
            if (prompt == null || prompt.trim().isEmpty()) {
                throw new IllegalArgumentException("非自定义模式下提示词不能为空");
            }
            if (prompt.length() > SunoMusicConstant.PROMPT_MAX_LENGTH_NON_CUSTOM) {
                throw new IllegalArgumentException("非自定义模式下提示词长度不能超过500个字符");
            }
        }
    }

    /**
     * 构建自定义模式翻唱请求
     */
    public static SunoUploadCoverRequest customMode(String uploadUrl, String style, String title,
                                                    Boolean instrumental, String prompt, SunoModelEnum model, String callBackUrl) {
        SunoUploadCoverRequest request = new SunoUploadCoverRequest();
        request.setUploadUrl(uploadUrl);
        request.setCustomMode(true);
        request.setStyle(style);
        request.setTitle(title);
        request.setInstrumental(instrumental);
        request.setPrompt(prompt);
        request.setModel(model);
        request.setCallBackUrl(callBackUrl);
        return request;
    }

    /**
     * 构建非自定义模式翻唱请求
     */
    public static SunoUploadCoverRequest simpleMode(String uploadUrl, String prompt, SunoModelEnum model, String callBackUrl) {
        SunoUploadCoverRequest request = new SunoUploadCoverRequest();
        request.setUploadUrl(uploadUrl);
        request.setCustomMode(false);
        request.setPrompt(prompt);
        request.setModel(model);
        request.setCallBackUrl(callBackUrl);
        request.setInstrumental(true); // 非自定义模式默认为纯音乐
        return request;
    }
}