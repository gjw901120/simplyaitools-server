package com.simply.ai.server.manager.model.request;

import com.simply.ai.server.manager.constant.SunoMusicConstant;
import com.simply.ai.server.manager.enums.SunoModelEnum;
import com.simply.ai.server.manager.enums.SunoVocalGenderEnum;
import lombok.Data;
import org.hibernate.validator.constraints.URL;

import javax.validation.constraints.*;
import java.io.Serializable;

/**
 * Suno基础请求参数
 */
@Data
public class SunoBaseRequest implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 用于生成的AI模型版本
     */
    @NotNull(message = "模型版本不能为空")
    private SunoModelEnum model;

    /**
     * 回调URL地址
     */
    @NotBlank(message = "回调URL不能为空")
    @URL(message = "回调URL格式不正确")
    private String callBackUrl;

    /**
     * 人声性别偏好
     */
    private SunoVocalGenderEnum vocalGender;

    /**
     * 对指定风格的遵循强度
     */
    @DecimalMin(value = "0.0", message = "风格权重不能小于0")
    @DecimalMax(value = "1.0", message = "风格权重不能大于1")
    @Digits(integer = 1, fraction = 2, message = "风格权重必须保留两位小数")
    private Double styleWeight;

    /**
     * 控制实验性/创意偏离程度
     */
    @DecimalMin(value = "0.0", message = "创意偏离度不能小于0")
    @DecimalMax(value = "1.0", message = "创意偏离度不能大于1")
    @Digits(integer = 1, fraction = 2, message = "创意偏离度必须保留两位小数")
    private Double weirdnessConstraint;

    /**
     * 音频要素相对权重
     */
    @DecimalMin(value = "0.0", message = "音频权重不能小于0")
    @DecimalMax(value = "1.0", message = "音频权重不能大于1")
    @Digits(integer = 1, fraction = 2, message = "音频权重必须保留两位小数")
    private Double audioWeight;

    /**
     * 从生成的音频中排除的音乐风格或特征
     */
    @Size(max = 500, message = "排除标签长度不能超过500个字符")
    private String negativeTags;

    /**
     * 验证权重参数
     */
    protected void validateWeightParameters() {
        if (styleWeight != null && Math.abs(styleWeight % SunoMusicConstant.WEIGHT_STEP) > 0.001) {
            throw new IllegalArgumentException("风格权重必须是0.01的倍数");
        }
        if (weirdnessConstraint != null && Math.abs(weirdnessConstraint % SunoMusicConstant.WEIGHT_STEP) > 0.001) {
            throw new IllegalArgumentException("创意偏离度必须是0.01的倍数");
        }
        if (audioWeight != null && Math.abs(audioWeight % SunoMusicConstant.WEIGHT_STEP) > 0.001) {
            throw new IllegalArgumentException("音频权重必须是0.01的倍数");
        }
    }

    /**
     * 获取模型对应的风格最大长度
     */
    protected int getStyleMaxLength() {
        return model != null ? model.getStyleMaxLength() : SunoMusicConstant.STYLE_MAX_LENGTH_V4_5_PLUS_V5;
    }

    /**
     * 获取模型对应的标题最大长度
     */
    protected int getTitleMaxLength() {
        return model != null ? model.getTitleMaxLength() : SunoMusicConstant.TITLE_MAX_LENGTH_V4_5_PLUS_V5;
    }

    /**
     * 获取模型对应的提示词最大长度
     */
    protected int getPromptMaxLength() {
        return model != null ? model.getPromptMaxLength() : SunoMusicConstant.PROMPT_MAX_LENGTH_V4_5_PLUS_V5;
    }

    /**
     * 获取模型枚举
     */
    protected SunoModelEnum getModel() {
        return model;
    }

    /**
     * 验证模型是否支持特定功能
     */
    protected void validateModelSupport(SunoModelEnum[] supportedModels, String featureName) {
        if (model == null) {
            throw new IllegalArgumentException("模型不能为空");
        }

        boolean supported = false;
        for (SunoModelEnum supportedModel : supportedModels) {
            if (model == supportedModel) {
                supported = true;
                break;
            }
        }

        if (!supported) {
            throw new IllegalArgumentException(featureName + "功能只支持" + getSupportedModelsString(supportedModels) + "模型");
        }
    }

    /**
     * 获取支持的模型字符串
     */
    private String getSupportedModelsString(SunoModelEnum[] supportedModels) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < supportedModels.length; i++) {
            if (i > 0) {
                sb.append("和");
            }
            sb.append(supportedModels[i].getCode());
        }
        return sb.toString();
    }
}